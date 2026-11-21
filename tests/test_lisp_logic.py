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
