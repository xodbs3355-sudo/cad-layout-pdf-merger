// zfmerge: 여러 PDF를 순서대로 한 파일로 합치는 프로그램.
//
// 사용법:
//
//	zfmerge.exe [--overwrite] [--delete] <결과.pdf> <입력.pdf 또는 폴더> ...
//
//	--overwrite : 결과 파일이 이미 있으면 덮어씀 (없으면 실패로 끝냄)
//	--delete    : 합치기에 성공하고 쪽수 검증까지 통과했을 때만 입력 PDF 삭제.
//	              입력이 폴더였으면, 비게 된 그 폴더도 삭제.
//
// 입력이 폴더면 그 안의 *.pdf 를 파일 이름 순서(001.pdf, 002.pdf ...)로 넣는다.
// 결과는 메시지 창으로 보여 주고, 실행 기록을 zfmerge_log.txt 에 남긴다.
package main

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/pdfcpu/pdfcpu/pkg/api"
	"github.com/pdfcpu/pdfcpu/pkg/pdfcpu/model"
)

type options struct {
	overwrite bool
	delete    bool
	out       string
	inputs    []string
}

func parseArgs(args []string) (options, error) {
	var o options
	var rest []string
	for _, a := range args {
		switch strings.ToLower(a) {
		case "--overwrite":
			o.overwrite = true
		case "--delete":
			o.delete = true
		default:
			rest = append(rest, a)
		}
	}
	if len(rest) < 2 {
		return o, errors.New("사용법: zfmerge.exe [--overwrite] [--delete] <결과.pdf> <입력.pdf 또는 폴더> ...")
	}
	o.out, o.inputs = rest[0], rest[1:]
	return o, nil
}

// expandInputs: 폴더는 안의 *.pdf 를 이름 순으로 펼친다. 결과 파일 자신은 제외.
func expandInputs(inputs []string, out string) (files []string, dirs []string, err error) {
	outAbs, _ := filepath.Abs(out)
	for _, in := range inputs {
		st, err := os.Stat(in)
		if err != nil {
			return nil, nil, fmt.Errorf("입력을 찾을 수 없음: %s (%v)", in, err)
		}
		if !st.IsDir() {
			files = append(files, in)
			continue
		}
		dirs = append(dirs, in)
		ents, err := os.ReadDir(in)
		if err != nil {
			return nil, nil, fmt.Errorf("폴더를 읽을 수 없음: %s (%v)", in, err)
		}
		var names []string
		for _, e := range ents {
			if !e.IsDir() && strings.EqualFold(filepath.Ext(e.Name()), ".pdf") {
				p := filepath.Join(in, e.Name())
				if a, _ := filepath.Abs(p); strings.EqualFold(a, outAbs) {
					continue
				}
				names = append(names, e.Name())
			}
		}
		sort.Strings(names)
		for _, n := range names {
			files = append(files, filepath.Join(in, n))
		}
	}
	if len(files) == 0 {
		return nil, nil, errors.New("합칠 PDF가 없습니다")
	}
	return files, dirs, nil
}

// run: 실제 작업. 기록할 줄들을 log 함수로 남기고, 사용자에게 보일 요약을 돌려준다.
func run(o options, log func(string)) (string, error) {
	t0 := time.Now()
	files, dirs, err := expandInputs(o.inputs, o.out)
	if err != nil {
		return "", err
	}
	if _, err := os.Stat(o.out); err == nil && !o.overwrite {
		return "", fmt.Errorf("같은 이름의 파일이 이미 있습니다: %s", o.out)
	}
	conf, err := api.LoadConfiguration(api.ConfigurationOptions{Mode: api.ConfigurationModeStateless})
	if err != nil {
		return "", fmt.Errorf("설정 준비 실패: %v", err)
	}
	conf.ValidationMode = model.ValidationRelaxed

	ctx := context.Background()
	want := 0
	for i, f := range files {
		n, err := api.PageCountFile(ctx, f)
		if err != nil {
			return "", fmt.Errorf("PDF를 읽을 수 없음: %s (%v)", f, err)
		}
		want += n
		log(fmt.Sprintf("  입력 %d: %s (%d쪽)", i+1, f, n))
	}

	// 임시 이름으로 만든 뒤 마지막에 이름을 바꿔, 중간에 실패해도 기존 결과 파일이 깨지지 않게 함
	tmp := o.out + ".zfmerge-tmp"
	_ = os.Remove(tmp)
	if err := api.MergeCreateFile(ctx, files, tmp, false, conf); err != nil {
		_ = os.Remove(tmp)
		return "", fmt.Errorf("합치기 실패: %v", err)
	}
	got, err := api.PageCountFile(ctx, tmp)
	if err != nil || got != want {
		_ = os.Remove(tmp)
		return "", fmt.Errorf("검증 실패: 결과 %d쪽, 예상 %d쪽 (%v)", got, want, err)
	}
	if err := os.Rename(tmp, o.out); err != nil {
		_ = os.Remove(tmp)
		return "", fmt.Errorf("결과 파일 저장 실패 (다른 프로그램에서 열려 있으면 닫고 다시 하세요): %s (%v)", o.out, err)
	}
	log(fmt.Sprintf("  결과: %s (%d쪽)", o.out, got))

	deleted := 0
	if o.delete {
		for _, f := range files {
			if err := os.Remove(f); err != nil {
				log(fmt.Sprintf("  삭제 실패(무시하고 계속): %s (%v)", f, err))
			} else {
				deleted++
			}
		}
		for _, d := range dirs {
			if err := os.Remove(d); err != nil { // 비어 있을 때만 지워짐
				log(fmt.Sprintf("  폴더 삭제 안 함: %s (%v)", d, err))
			}
		}
	}
	msg := fmt.Sprintf("합치기 완료: %d개 파일, %d쪽\n%s\n소요 시간 %.1f초",
		len(files), got, o.out, time.Since(t0).Seconds())
	if o.delete {
		msg += fmt.Sprintf("\n개별 PDF 삭제: %d/%d개", deleted, len(files))
	}
	return msg, nil
}

// logPath: 실행 파일 옆 zfmerge_log.txt. 쓸 수 없으면 임시 폴더.
func logPath() string {
	if exe, err := os.Executable(); err == nil {
		p := filepath.Join(filepath.Dir(exe), "zfmerge_log.txt")
		if f, err := os.OpenFile(p, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644); err == nil {
			f.Close()
			return p
		}
	}
	return filepath.Join(os.TempDir(), "zfmerge_log.txt")
}

func main() {
	lp := logPath()
	var lines []string
	log := func(s string) { lines = append(lines, s) }
	log(fmt.Sprintf("[%s] zfmerge 실행: %q", time.Now().Format("2006-01-02 15:04:05"), os.Args[1:]))

	code := 0
	o, err := parseArgs(os.Args[1:])
	var msg string
	if err == nil {
		msg, err = run(o, log)
	}
	if err != nil {
		code = 1
		msg = "오류: " + err.Error()
	}
	log("  " + strings.ReplaceAll(msg, "\n", " / "))

	if f, ferr := os.OpenFile(lp, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644); ferr == nil {
		f.WriteString(strings.Join(lines, "\r\n") + "\r\n")
		f.Close()
	} else {
		msg += "\n(기록 파일을 쓸 수 없음: " + ferr.Error() + ")"
	}
	showMessage("PDF 합치기 (zfmerge)", msg+"\n\n기록: "+lp, code != 0)
	os.Exit(code)
}
