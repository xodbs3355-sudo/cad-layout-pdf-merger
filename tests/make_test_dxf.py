"""ezdxf로 시험용 도면 생성.

배치탭(용지 공간)에 프레임을 한 줄 5장 격자로 배치하고, 각 모서리 바깥에
ㄱ자 표시(점 3개 LWPOLYLINE, 레이어 "0", 색 2 노란색, 팔 길이 40)를 그린다.
실제 도면을 흉내 내기 위해
- 좌표에 ±0.1 이하 무작위 어긋남, 점 순서(방향) 무작위, 생성 순서 무작위
- 도곽(프레임 테두리)은 같은 레이어 "0"의 닫힌 폴리선
- 걸러져야 할 방해 객체: 팔 길이 다른 ㄱ자, 색 다른 ㄱ자, 닫힌 삼각형,
  곡선 구간 있는 폴리선, 가로+세로가 아닌 폴리선
을 넣는다. 이 DXF는 AutoCAD에서 열어 LISP 시험용으로도 쓸 수 있다.

엔터티 표현(dict): pts=[(x,y)...], color=정수 또는 None(BYLAYER), closed, bulges
"""
import random
import sys

import ezdxf

W, H = 420.0, 297.0      # 프레임 크기 (시험용 가정값, 로직은 이 값에 의존하지 않음)
GAP_X, GAP_Y = 60.0, 100.0
ARM = 40.0               # ㄱ자 팔 길이 (사용자 도면 LIST 결과 기준)
COLOR = 2
JIT = 0.1


def frame_origins(count, per_row=5):
    """Z 순서대로 각 프레임의 왼쪽아래 좌표"""
    out = []
    for i in range(count):
        r, c = divmod(i, per_row)
        out.append((c * (W + GAP_X), -r * (H + GAP_Y)))
    return out


def ent(pts, color=COLOR, closed=False, bulges=None):
    return {"pts": [tuple(p) for p in pts], "color": color, "closed": closed,
            "bulges": bulges or [0.0] * len(pts)}


def marks(x, y, rnd, skip=()):
    """프레임 (x,y)-(x+W,y+H) 의 ㄱ자 4개. skip에 'TL' 등을 넣으면 그 모서리 생략."""
    j = lambda: rnd.uniform(-JIT, JIT)
    corners = {
        "TL": ((x, y + H), (-1, +1)),
        "TR": ((x + W, y + H), (+1, +1)),
        "BL": ((x, y), (-1, -1)),
        "BR": ((x + W, y), (+1, -1)),
    }
    out = []
    for name, ((cx, cy), (sx, sy)) in corners.items():
        if name in skip:
            continue
        cx, cy = cx + j(), cy + j()
        pts = [(cx + sx * ARM, cy + j() * 0.5), (cx, cy), (cx + j() * 0.5, cy + sy * ARM)]
        if rnd.random() < 0.5:
            pts.reverse()
        out.append(ent(pts))
    return out


def distractors():
    """걸러져야 하는 객체들 (프레임 격자 위쪽 빈 곳에 배치)"""
    bx, by = 0.0, H + 300.0
    return [
        ent([(bx + 40, by), (bx, by), (bx, by + 20)]),                    # 팔 길이 다름
        ent([(bx + 140, by), (bx + 100, by), (bx + 100, by + 40)], color=1),  # 색 다름
        ent([(bx + 240, by), (bx + 200, by), (bx + 200, by + 40)], closed=True),  # 닫힘
        ent([(bx + 340, by), (bx + 300, by), (bx + 300, by + 40)],
            bulges=[0.5, 0.0, 0.0]),                                        # 곡선 구간
        ent([(bx + 440, by), (bx + 400, by), (bx + 430, by + 40)]),        # 가로+세로 아님
    ]


EXPECTED_REJECTS = {"팔 길이 다름": 1, "닫힌 선": 1, "곡선 구간 있음": 1, "가로+세로 모양 아님": 1}


def build(count=13, seed=1, skip_map=None, path=None, with_distractors=True):
    rnd = random.Random(seed)
    skip_map = skip_map or {}
    origins = frame_origins(count)
    ents = []
    for i, (x, y) in enumerate(origins):
        ents += marks(x, y, rnd, skip_map.get(i, ()))
    if with_distractors:
        ents += distractors()
    rnd.shuffle(ents)

    if path:
        doc = ezdxf.new("R2018", setup=True)
        psp = doc.layouts.get("Layout1")
        for (x, y) in origins:
            psp.add_lwpolyline([(x, y), (x + W, y), (x + W, y + H), (x, y + H)],
                               close=True, dxfattribs={"layer": "0"})
        for e in ents:
            pts = [(p[0], p[1], 0, 0, b) for p, b in zip(e["pts"], e["bulges"])]
            attrs = {"layer": "0"}
            if e["color"] is not None:
                attrs["color"] = e["color"]
            psp.add_lwpolyline(pts, format="xyseb", close=e["closed"], dxfattribs=attrs)
        doc.saveas(path)
    return ents, origins


if __name__ == "__main__":
    out = sys.argv[1] if len(sys.argv) > 1 else "zframe_test.dxf"
    build(path=out)
    print("saved", out)
