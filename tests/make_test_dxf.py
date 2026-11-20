"""ezdxf로 시험용 도면 생성.

배치탭(용지 공간)에 A3 크기 프레임을 한 줄 5장 격자로 배치하고,
각 모서리 바깥에 ㄱ자 표시(LINE 2개)를 그린다. 실제 도면을 흉내 내기 위해
- 좌표에 ±0.1 이하 무작위 어긋남
- 선의 시작/끝점 방향 무작위
- 선 생성 순서 무작위
- 도곽(프레임 테두리)은 다른 레이어의 폴리선
을 넣는다. 이 DXF는 AutoCAD에서 열어 LISP 시험용으로도 쓸 수 있다.
"""
import random
import sys

import ezdxf

MARK_LAYER = "PLOT_MARK"
W, H = 420.0, 297.0      # 프레임 크기 (시험용 가정값, 로직은 이 값에 의존하지 않음)
GAP_X, GAP_Y = 60.0, 80.0
L = 10.0                 # ㄱ자 선 길이
JIT = 0.1


def frame_origins(count, per_row=5):
    """Z 순서대로 각 프레임의 왼쪽아래 좌표"""
    out = []
    for i in range(count):
        r, c = divmod(i, per_row)
        out.append((c * (W + GAP_X), -r * (H + GAP_Y)))
    return out


def mark_lines(x, y, rnd, skip=()):
    """프레임 (x,y)-(x+W,y+H) 의 ㄱ자 표시 선 목록. skip에 'TL' 등을 넣으면 그 모서리 생략."""
    j = lambda: rnd.uniform(-JIT, JIT)
    corners = {
        "TL": ((x, y + H), (-1, +1)),
        "TR": ((x + W, y + H), (+1, +1)),
        "BL": ((x, y), (-1, -1)),
        "BR": ((x + W, y), (+1, -1)),
    }
    lines = []
    for name, ((cx, cy), (sx, sy)) in corners.items():
        if name in skip:
            continue
        cx, cy = cx + j(), cy + j()
        hl = [(cx, cy), (cx + sx * L, cy + j() * 0.5)]
        vl = [(cx + j() * 0.5, cy), (cx, cy + sy * L)]
        for ln in (hl, vl):
            if rnd.random() < 0.5:
                ln.reverse()
            lines.append(tuple(ln))
    return lines


def build(count=13, seed=1, skip_map=None, path=None):
    rnd = random.Random(seed)
    skip_map = skip_map or {}
    origins = frame_origins(count)
    lines = []
    for i, (x, y) in enumerate(origins):
        lines += mark_lines(x, y, rnd, skip_map.get(i, ()))
    rnd.shuffle(lines)

    if path:
        doc = ezdxf.new("R2018", setup=True)
        doc.layers.add(MARK_LAYER, color=2)   # 노란색
        doc.layers.add("FRAME", color=7)
        psp = doc.layouts.get("Layout1")
        for (x, y) in origins:
            psp.add_lwpolyline([(x, y), (x + W, y), (x + W, y + H), (x, y + H)],
                               close=True, dxfattribs={"layer": "FRAME"})
        for p1, p2 in lines:
            psp.add_line(p1, p2, dxfattribs={"layer": MARK_LAYER})
        doc.saveas(path)
    return lines, origins


if __name__ == "__main__":
    out = sys.argv[1] if len(sys.argv) > 1 else "zframe_test.dxf"
    build(path=out)
    print("saved", out)
