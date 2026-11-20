"""zframe 로직 시험. 실행: python -m pytest tests"""
import os
import sys

import ezdxf

sys.path.insert(0, os.path.dirname(__file__))
import make_test_dxf as M  # noqa: E402
import zframe_algo as Z    # noqa: E402


def lines_from_dxf(path, layer, layout="Layout1"):
    doc = ezdxf.readfile(path)
    out = []
    for e in doc.layouts.get(layout).query(f'LINE[layer=="{layer}"]'):
        out.append(((e.dxf.start.x, e.dxf.start.y), (e.dxf.end.x, e.dxf.end.y)))
    return out


def check_order(frames, origins):
    assert len(frames) == len(origins)
    for f, (x, y) in zip(frames, origins):
        assert abs(f[0] - x) < 0.5 and abs(f[1] - y) < 0.5


def test_full_grid_via_dxf(tmp_path):
    p = str(tmp_path / "t.dxf")
    _, origins = M.build(count=13, seed=1, path=p)
    lines = lines_from_dxf(p, M.MARK_LAYER)
    assert len(lines) == 13 * 8
    frames, fails, corners = Z.run(lines)
    assert fails == []
    assert len(corners) == 13 * 4
    check_order(frames, origins)


def test_many_seeds_and_counts():
    for seed in range(30):
        for count in (1, 4, 5, 6, 23, 40):
            lines, origins = M.build(count=count, seed=seed)
            frames, fails, _ = Z.run(lines)
            assert fails == [], (seed, count)
            check_order(frames, origins)


def test_missing_top_right_is_reported_not_mispaired():
    # 2번 프레임(인덱스 1)의 오른쪽위 표시를 빼면, 왼쪽위가 3번 프레임의 오른쪽위와
    # 잘못 짝지어지지 않고 실패로 보고되어야 함
    lines, origins = M.build(count=10, seed=3, skip_map={1: ("TR",)})
    frames, fails, _ = Z.run(lines)
    assert len(frames) == 9
    assert len(fails) == 1
    fx, fy, _ = fails[0]
    assert abs(fx - origins[1][0]) < 0.5 and abs(fy - (origins[1][1] + M.H)) < 0.5


def test_missing_bottom_left_is_reported():
    lines, _ = M.build(count=10, seed=4, skip_map={2: ("BL",)})
    frames, fails, _ = Z.run(lines)
    assert len(frames) == 9 and len(fails) == 1
