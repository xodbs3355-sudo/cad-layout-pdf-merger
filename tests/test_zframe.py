"""zframe 로직(Python 이식본) 시험. 실행: python -m pytest tests"""
import os
import sys

import ezdxf

sys.path.insert(0, os.path.dirname(__file__))
import make_test_dxf as M  # noqa: E402
import zframe_algo as Z    # noqa: E402


def is_mark(e):
    return isinstance(Z.mark_shape(e, 0.5), tuple)


def sample_of(ents):
    """사용자가 클릭할 견본: 첫 번째 정상 ㄱ자"""
    return next(e for e in ents if e["color"] == M.COLOR and is_mark(e)
                and abs(Z.mark_shape(e, 0.5)[2] - M.ARM) < 0.5)


def ents_from_dxf(path, layout="Layout1"):
    doc = ezdxf.readfile(path)
    out = []
    for e in doc.layouts.get(layout).query("LWPOLYLINE"):
        pts = list(e.get_points("xyb"))
        color = e.dxf.color if e.dxf.hasattr("color") and e.dxf.color != 256 else None
        out.append({"pts": [(p[0], p[1]) for p in pts], "bulges": [p[2] for p in pts],
                    "closed": e.closed, "color": color})
    return out


def check_order(frames, origins):
    assert len(frames) == len(origins)
    for f, (x, y) in zip(frames, origins):
        assert abs(f[0] - x) < 0.5 and abs(f[1] - y) < 0.5


def test_full_grid_via_dxf(tmp_path):
    p = str(tmp_path / "t.dxf")
    M.build(count=13, seed=1, path=p)
    _, origins = M.build(count=13, seed=1)
    ents = ents_from_dxf(p)
    frames, fails, corners, cnt, rej = Z.run_marks(ents, sample_of(ents))
    assert fails == []
    assert len(corners) == 13 * 4
    assert rej == M.EXPECTED_REJECTS          # 색 다른 것은 필터에서 이미 제외
    assert cnt == 13 * 4 + 4
    check_order(frames, origins)


def test_many_seeds_and_counts():
    for seed in range(30):
        for count in (1, 4, 5, 6, 23, 40):
            ents, origins = M.build(count=count, seed=seed)
            frames, fails, _, _, rej = Z.run_marks(ents, sample_of(ents))
            assert fails == [], (seed, count)
            assert rej == M.EXPECTED_REJECTS
            check_order(frames, origins)


def test_missing_top_right_is_reported_not_mispaired():
    # 2번 프레임(인덱스 1)의 오른쪽위 표시를 빼면 이웃 것과 잘못 짝지어지지 않고 실패로 보고
    ents, origins = M.build(count=10, seed=3, skip_map={1: ("TR",)})
    frames, fails, _, _, _ = Z.run_marks(ents, sample_of(ents))
    assert len(frames) == 9 and len(fails) == 1
    fx, fy, _ = fails[0]
    assert abs(fx - origins[1][0]) < 0.5 and abs(fy - (origins[1][1] + M.H)) < 0.5


def test_missing_bottom_left_is_reported():
    ents, _ = M.build(count=10, seed=4, skip_map={2: ("BL",)})
    frames, fails, _, _, _ = Z.run_marks(ents, sample_of(ents))
    assert len(frames) == 9 and len(fails) == 1


def test_bylayer_sample_disables_color_filter():
    # 견본이 BYLAYER(color None)이면 색 조건 없이 찾으므로 색 1 방해 객체도 후보에 포함됨
    ents, _ = M.build(count=5, seed=5)
    samp = dict(sample_of(ents), color=None)
    _, _, _, cnt, _ = Z.run_marks(ents, samp)
    assert cnt == 5 * 4 + 5
