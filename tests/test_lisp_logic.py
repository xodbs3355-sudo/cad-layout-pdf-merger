"""zframe.lsp 의 계산 함수를 초소형 해석기로 직접 실행해 Python 버전과 결과 비교."""
import os
import sys

HERE = os.path.dirname(__file__)
sys.path.insert(0, HERE)
import make_test_dxf as M  # noqa: E402
import zframe_algo as Z    # noqa: E402
from mini_autolisp import Interp, Sym, L  # noqa: E402

LSP = os.path.join(HERE, "..", "lisp", "zframe.lsp")


def lisp_run(lines, tol_pt=0.5, tol_row=5.0):
    it = Interp()
    with open(LSP, encoding="utf-8") as f:
        it.load(f.read())
    call = lambda name, *a: it.g[Sym(name.upper())](*a)
    lisp_lines = L([[list(p1), list(p2)] for p1, p2 in lines])
    hv = call("zf-split", lisp_lines, tol_pt)
    corners = call("zf-find-corners", hv[0], hv[1], tol_pt)
    built = call("zf-build-frames", corners, tol_pt)
    frames = call("zf-zorder", built[0], tol_row) or []
    fails = built[1] or []
    return [tuple(f) for f in frames], fails


def test_lisp_matches_python_and_expected():
    for seed in range(10):
        for count in (1, 5, 7, 13, 23):
            lines, origins = M.build(count=count, seed=seed)
            lf, lfails = lisp_run(lines)
            pf, pfails, _ = Z.run(lines)
            assert lfails == [] and pfails == []
            assert len(lf) == count
            for a, b, (x, y) in zip(lf, pf, origins):
                assert all(abs(u - v) < 1e-9 for u, v in zip(a, b))
                assert abs(a[0] - x) < 0.5 and abs(a[1] - y) < 0.5


def test_lisp_reports_missing_corner():
    lines, _ = M.build(count=10, seed=3, skip_map={1: ("TR",)})
    lf, lfails = lisp_run(lines)
    assert len(lf) == 9 and len(lfails) == 1
