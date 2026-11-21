"""zframe.lsp 의 계산 함수를 초소형 해석기로 직접 실행해 Python 버전·정답과 비교.

ssget / ssname / sslength / entget 은 가짜(mock)로 대신한다.
- ssget "X" : 필터의 모든 (코드 . 값)이 같은 객체만 고름. 하나도 없으면 nil (실제 동작과 동일)
- entget    : 실제 LWPOLYLINE과 같은 순서의 DXF 목록 반환
"""
import os
import sys

HERE = os.path.dirname(__file__)
sys.path.insert(0, HERE)
import make_test_dxf as M  # noqa: E402
import zframe_algo as Z    # noqa: E402
from mini_autolisp import Interp, Sym, L, Dotted  # noqa: E402

LSP = os.path.join(HERE, "..", "lisp", "zframe.lsp")
TAB = "배치1"


def to_ed(e, tab=TAB):
    ed = [Dotted((0, "LWPOLYLINE")), Dotted((410, tab)), Dotted((8, "0")),
          Dotted((90, len(e["pts"]))), Dotted((70, 1 if e["closed"] else 0))]
    if e["color"] is not None:
        ed.append(Dotted((62, e["color"])))
    for (x, y), b in zip(e["pts"], e["bulges"]):
        ed += [[10, x, y], Dotted((40, 0.0)), Dotted((41, 0.0)), Dotted((42, b))]
    return ed


def make_interp(eds):
    it = Interp()
    with open(LSP, encoding="utf-8") as f:
        it.load(f.read())

    def ssget(mode, flt):
        assert mode == "X"
        conds = [(c[0], c[1]) for c in flt]
        hit = [ed for ed in eds
               if all(any(isinstance(p, Dotted) and p[0] == k and p[1] == v for p in ed)
                      for k, v in conds)]
        return hit or None

    it.g[Sym("SSGET")] = ssget
    it.g[Sym("SSLENGTH")] = lambda ss: len(ss)
    it.g[Sym("SSNAME")] = lambda ss, i: ss[i] if i < len(ss) else None
    it.g[Sym("ENTGET")] = lambda en: en
    return it


def lisp_run(ents, samp_ent, tol_pt=0.5, tol_row=5.0):
    eds = [to_ed(e) for e in ents]
    it = make_interp(eds)
    call = lambda name, *a: it.g[Sym(name.upper())](*a)
    samp_ed = to_ed(samp_ent)
    samp = call("zf-mark-shape", samp_ed, tol_pt)
    color = (call("assoc", 62, samp_ed) or [None, None])[1]
    got = call("zf-get-marks", TAB, samp, color, tol_pt)
    lines, cnt, rej = got[0], got[1], got[2]
    hv = call("zf-split", lines, tol_pt)
    corners = call("zf-find-corners", hv[0], hv[1], tol_pt)
    built = call("zf-build-frames", corners, tol_pt)
    frames = call("zf-zorder", built[0], tol_row) or []
    return [tuple(f) for f in frames], built[1] or [], cnt, dict(rej or [])


def sample_of(ents):
    return next(e for e in ents if e["color"] == M.COLOR
                and isinstance(Z.mark_shape(e, 0.5), tuple)
                and abs(Z.mark_shape(e, 0.5)[2] - M.ARM) < 0.5)


def test_sample_shape():
    # 사용자 도면 LIST 결과의 ㄱ자 그대로
    e = M.ent([(886.6168, 43.8429), (846.6168, 43.8429), (846.6168, 3.8429)])
    it = make_interp([])
    r = it.g[Sym("ZF-MARK-SHAPE")](to_ed(e), 0.5)
    assert abs(r[2] - 40) < 1e-9 and abs(r[3] - 40) < 1e-9
    # 꼭짓점(가운데 점)에서 출발하는 두 선
    assert r[0][0] == [846.6168, 43.8429] or tuple(r[0][0]) == (846.6168, 43.8429)


def test_lisp_matches_python_and_expected():
    for seed in range(8):
        for count in (1, 5, 7, 13, 23):
            ents, origins = M.build(count=count, seed=seed)
            samp = sample_of(ents)
            lf, lfails, lcnt, lrej = lisp_run(ents, samp)
            pf, pfails, _, pcnt, prej = Z.run_marks(ents, samp)
            assert lfails == [] and pfails == []
            assert lcnt == pcnt == count * 4 + 4
            assert lrej == prej == M.EXPECTED_REJECTS
            assert len(lf) == count
            for a, b, (x, y) in zip(lf, pf, origins):
                assert all(abs(u - v) < 1e-9 for u, v in zip(a, b))
                assert abs(a[0] - x) < 0.5 and abs(a[1] - y) < 0.5


def test_lisp_reports_missing_corner():
    ents, _ = M.build(count=10, seed=3, skip_map={1: ("TR",)})
    lf, lfails, _, _ = lisp_run(ents, sample_of(ents))
    assert len(lf) == 9 and len(lfails) == 1


def test_lisp_no_candidates_returns_empty():
    # ssget 이 nil 을 돌려줄 때 오류 없이 0개
    ents, _ = M.build(count=3, seed=1)
    samp = dict(sample_of(ents), color=7)    # 색 7인 객체는 없음
    lf, lfails, cnt, rej = lisp_run(ents, samp)
    assert lf == [] and lfails == [] and cnt == 0 and rej == {}


def test_zf_detect_matches_pipeline():
    ents, origins = M.build(count=13, seed=1)
    it = make_interp([to_ed(e) for e in ents])
    samp_ed = to_ed(sample_of(ents))
    samp = it.g[Sym("ZF-MARK-SHAPE")](samp_ed, 0.5)
    det = it.g[Sym("ZF-DETECT")](TAB, samp, M.COLOR)
    ordered, fails = det[2], det[3]
    assert fails is None and len(ordered) == 13
    for f, (x, y) in zip(ordered, origins):
        assert abs(f[0] - x) < 0.5 and abs(f[1] - y) < 0.5


def test_strip_ext():
    it = make_interp([])
    strip = it.g[Sym("ZF-STRIP-EXT")]
    assert strip("a.b.dwg") == "a.b" and strip("noext") == "noext"


def test_parse_sel():
    it = make_interp([])
    ps = it.g[Sym("ZF-PARSE-SEL")]
    assert ps("", 4) == [1, 2, 3, 4]
    assert ps(" 1-3, 8,2 ", 10) == [1, 2, 3, 8]
    assert ps("5", 5) == [5]
    assert isinstance(ps("0-2", 5), str)
    assert isinstance(ps("4-12", 10), str)
    assert isinstance(ps("a", 5), str)
    assert isinstance(ps("3-1", 5), str)
    assert isinstance(ps("1,,2", 5), str)


def run_pdfmerge(ents, answers_str, out_path, kwords, existing=()):
    """c:PDFMERGE 전체 흐름을 가짜 AutoCAD 함수로 실행. 기록(dict) 반환"""
    eds = [to_ed(e) for e in ents]
    it = make_interp(eds)
    samp_ed = to_ed(sample_of(ents))
    rec = {"plots": [], "startapp": [], "princ": [], "setvar": [], "entmake": 0}
    files = set(existing) | {"zfmerge.exe"}
    clock = [0]
    sysv = {"TILEMODE": 0, "CVPORT": 1, "CTAB": TAB, "DWGPREFIX": "C:\\d\\", "DWGNAME": "도면.dwg",
            "CDATE": 20261122.012345, "FILEDIA": 1, "OSMODE": 4133, "BACKGROUNDPLOT": 2,
            "CMDACTIVE": 0, "DATE": 2461000.5}

    def getvar(n):
        if n == "MILLISECS":
            clock[0] += 500
            return clock[0]
        return sysv.get(n)

    def setvar(n, v):
        rec["setvar"].append((n, v))
        sysv[n] = v
        return v

    def command(*a):
        if a and a[0] == "-PLOT":
            assert len(a) == 22, a
            assert a[8] == "W" and a[11] == "F" and a[12] == "C"
            assert sysv["FILEDIA"] == 0 and sysv["OSMODE"] == 0 and sysv["BACKGROUNDPLOT"] == 0
            rec["plots"].append((a[9], a[10], a[19]))
            files.add(a[19])
        return None

    sq, kq = list(answers_str), list(kwords)
    g = it.g
    g[Sym("GETVAR")] = getvar
    g[Sym("SETVAR")] = setvar
    g[Sym("COMMAND")] = command
    g[Sym("FINDFILE")] = lambda p: ("C:\\tools\\zfmerge.exe" if p == "zfmerge.exe" else p) if p in files else None
    g[Sym("ENTSEL")] = lambda *a: [samp_ed, [0, 0, 0]]
    g[Sym("GETSTRING")] = lambda *a: sq.pop(0)
    g[Sym("GETFILED")] = lambda *a: out_path
    g[Sym("INITGET")] = lambda *a: None
    g[Sym("GETKWORD")] = lambda *a: kq.pop(0)
    g[Sym("STARTAPP")] = lambda *a: rec["startapp"].append(a) or 33
    g[Sym("TBLSEARCH")] = lambda *a: True
    g[Sym("ENTMAKE")] = lambda e: rec.__setitem__("entmake", rec["entmake"] + 1) or e
    g[Sym("ENTDEL")] = lambda e: e
    g[Sym("PRINC")] = lambda *a: rec["princ"].append(a[0] if a else "") or NIL_
    g[Sym("C:PDFMERGE")]()
    rec["sysv"] = sysv
    rec["files"] = files
    return rec


NIL_ = None


def test_zfp_all_frames_flow():
    ents, origins = M.build(count=7, seed=2)
    rec = run_pdfmerge(ents, [""], "C:\\out\\합친 도면.pdf", [None])
    assert len(rec["plots"]) == 7
    # Z 순서대로 출력, 임시 파일은 결과 폴더에
    for (p1, p2, path), (x, y) in zip(rec["plots"], origins):
        assert abs(p1[0] - x) < 0.5 and abs(p1[1] - y) < 0.5
        assert path.startswith("C:\\out\\zf_012345_")
    assert rec["plots"][0][2].endswith("_001.pdf")
    # 시스템 변수 복원
    assert rec["sysv"]["FILEDIA"] == 1 and rec["sysv"]["OSMODE"] == 4133 and rec["sysv"]["BACKGROUNDPLOT"] == 2
    # 합치기 호출
    assert len(rec["startapp"]) == 1
    exe, args = rec["startapp"][0]
    assert exe == "C:\\tools\\zfmerge.exe" or exe.endswith("zfmerge.exe")
    assert args.startswith('--delete "C:\\out\\합친 도면.pdf"')
    assert args.count(".pdf") == 8 and "--overwrite" not in args


def test_zfp_selection_and_overwrite():
    ents, origins = M.build(count=7, seed=2)
    out = "C:\\out\\a.pdf"
    rec = run_pdfmerge(ents, ["x", "2-3,6"], out, ["Y", None], existing={out})
    assert len(rec["plots"]) == 3
    xs = [round(p[0][0]) for p in rec["plots"]]
    assert [round(origins[i][0]) for i in (1, 2, 5)] == xs
    assert rec["startapp"][0][1].startswith('--overwrite --delete')


def test_zfp_overwrite_declined_cancels():
    ents, _ = M.build(count=3, seed=2)
    out = "C:\\out\\a.pdf"
    rec = run_pdfmerge(ents, [""], out, [None], existing={out})   # Enter = 기본값 N
    assert rec["plots"] == [] and rec["startapp"] == []


def test_zfp_confirm_no_cancels():
    ents, _ = M.build(count=3, seed=2)
    rec = run_pdfmerge(ents, [""], "C:\\out\\b.pdf", ["N"])
    assert rec["plots"] == [] and rec["startapp"] == []


def test_find_in_trusted():
    it = make_interp([])
    tp = "C:\\x\\a;;C:\\Users\\l\\Downloads\\cad-layout-pdf-merger-main\\lisp\\...;D:\\b\\"
    want = "C:\\Users\\l\\Downloads\\cad-layout-pdf-merger-main\\lisp\\zfmerge.exe"
    it.g[Sym("GETVAR")] = lambda n: tp if n == "TRUSTEDPATHS" else None
    it.g[Sym("FINDFILE")] = lambda p: p if p == want else None
    assert it.g[Sym("ZF-FIND-IN-TRUSTED")]() == want
    it.g[Sym("FINDFILE")] = lambda p: None
    assert it.g[Sym("ZF-FIND-IN-TRUSTED")]() is None
    it.g[Sym("GETVAR")] = lambda n: None
    assert it.g[Sym("ZF-FIND-IN-TRUSTED")]() is None
