package main

import (
	"context"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"github.com/pdfcpu/pdfcpu/pkg/api"
)

// 시험용 PDF: 1쪽짜리를 pdfcpu의 빈 페이지 기능 대신, 최소 PDF 문자열로 직접 만든다.
// 쪽 크기를 파일마다 다르게 해서 합친 뒤 순서를 확인한다.
func writeMinimalPDF(t *testing.T, path string, width int) {
	t.Helper()
	objs := []string{
		"<< /Type /Catalog /Pages 2 0 R >>",
		"<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
		"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 " + itoa(width) + " 300] >>",
	}
	var b strings.Builder
	b.WriteString("%PDF-1.4\n")
	offs := []int{}
	for i, o := range objs {
		offs = append(offs, b.Len())
		b.WriteString(itoa(i+1) + " 0 obj\n" + o + "\nendobj\n")
	}
	xref := b.Len()
	b.WriteString("xref\n0 " + itoa(len(objs)+1) + "\n0000000000 65535 f \n")
	for _, o := range offs {
		s := itoa(o)
		b.WriteString(strings.Repeat("0", 10-len(s)) + s + " 00000 n \n")
	}
	b.WriteString("trailer\n<< /Size " + itoa(len(objs)+1) + " /Root 1 0 R >>\nstartxref\n" + itoa(xref) + "\n%%EOF\n")
	if err := os.WriteFile(path, []byte(b.String()), 0o644); err != nil {
		t.Fatal(err)
	}
}

func itoa(i int) string { return strconv.Itoa(i) }

func pageWidths(t *testing.T, path string) []float64 {
	t.Helper()
	c := context.Background()
	ctx, err := api.ReadContextFile(c, path)
	if err != nil {
		t.Fatal(err)
	}
	dims, err := ctx.PageDims(c)
	if err != nil {
		t.Fatal(err)
	}
	var w []float64
	for _, d := range dims {
		w = append(w, d.Width)
	}
	return w
}

func noLog(string) {}

func TestMergeFolderInNameOrderAndDelete(t *testing.T) {
	dir := t.TempDir()
	in := filepath.Join(dir, "임시 폴더")
	os.Mkdir(in, 0o755)
	// 일부러 이름과 다른 순서로 생성
	writeMinimalPDF(t, filepath.Join(in, "003.pdf"), 330)
	writeMinimalPDF(t, filepath.Join(in, "001.pdf"), 310)
	writeMinimalPDF(t, filepath.Join(in, "002.pdf"), 320)
	out := filepath.Join(dir, "결과 도면.pdf")

	msg, err := run(options{delete: true, out: out, inputs: []string{in}}, noLog)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(msg, "3쪽") {
		t.Fatalf("msg: %s", msg)
	}
	w := pageWidths(t, out)
	if len(w) != 3 || w[0] != 310 || w[1] != 320 || w[2] != 330 {
		t.Fatalf("order: %v", w)
	}
	if _, err := os.Stat(in); !os.IsNotExist(err) {
		t.Fatalf("input folder should be removed: %v", err)
	}
}

func TestRefuseOverwriteWithoutFlag(t *testing.T) {
	dir := t.TempDir()
	a := filepath.Join(dir, "a.pdf")
	writeMinimalPDF(t, a, 300)
	out := filepath.Join(dir, "out.pdf")
	os.WriteFile(out, []byte("old"), 0o644)
	if _, err := run(options{out: out, inputs: []string{a}}, noLog); err == nil {
		t.Fatal("expected error")
	}
	if b, _ := os.ReadFile(out); string(b) != "old" {
		t.Fatal("existing file must not change")
	}
	if _, err := os.Stat(a); err != nil {
		t.Fatal("input must remain")
	}
	if _, err := run(options{overwrite: true, out: out, inputs: []string{a}}, noLog); err != nil {
		t.Fatal(err)
	}
	if w := pageWidths(t, out); len(w) != 1 {
		t.Fatalf("pages: %v", w)
	}
}

func TestBadInputKeepsFiles(t *testing.T) {
	dir := t.TempDir()
	a := filepath.Join(dir, "a.pdf")
	writeMinimalPDF(t, a, 300)
	bad := filepath.Join(dir, "b.pdf")
	os.WriteFile(bad, []byte("not a pdf"), 0o644)
	out := filepath.Join(dir, "out.pdf")
	if _, err := run(options{delete: true, out: out, inputs: []string{a, bad}}, noLog); err == nil {
		t.Fatal("expected error")
	}
	for _, p := range []string{a, bad} {
		if _, err := os.Stat(p); err != nil {
			t.Fatalf("input deleted on failure: %s", p)
		}
	}
	if _, err := os.Stat(out); !os.IsNotExist(err) {
		t.Fatal("no output on failure")
	}
}

func TestParseArgs(t *testing.T) {
	o, err := parseArgs([]string{"--DELETE", "out.pdf", "d"})
	if err != nil || !o.delete || o.overwrite || o.out != "out.pdf" || len(o.inputs) != 1 {
		t.Fatalf("%+v %v", o, err)
	}
	if _, err := parseArgs([]string{"out.pdf"}); err == nil {
		t.Fatal("expected usage error")
	}
}
