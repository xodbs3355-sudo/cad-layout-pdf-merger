"""lisp/zframe.lsp 의 프레임 찾기·Z 정렬 로직을 Python으로 1:1 옮긴 것.

목적: LISP를 AutoCAD 없이 시험할 수 없으므로, 같은 알고리즘을 Python으로 돌려
로직 자체(짝짓기·정렬)가 맞는지 확인한다. LISP 코드 자체를 실행하는 것은 아님.
"""
import math


def near(a, b, tol):
    return abs(a - b) <= tol


def split(lines, tol):
    h, v = [], []
    for p1, p2 in lines:
        dx, dy = abs(p2[0] - p1[0]), abs(p2[1] - p1[1])
        if dy <= tol and dx > tol:
            h.append((p1, p2))
        elif dx <= tol and dy > tol:
            v.append((p1, p2))
    return h, v


def find_corners(hs, vs, tol):
    res = []
    for h in hs:
        for v in vs:
            for hi in (0, 1):
                for vi in (0, 1):
                    hp, ho = h[hi], h[1 - hi]
                    vp, vo = v[vi], v[1 - vi]
                    if math.dist(hp, vp) <= tol:
                        if vo[1] > vp[1]:
                            typ = "TL" if ho[0] < hp[0] else "TR"
                        else:
                            typ = "BL" if ho[0] < hp[0] else "BR"
                        c = (typ, vp[0], hp[1])
                        if not any(e[0] == c[0] and near(e[1], c[1], tol) and near(e[2], c[2], tol)
                                   for e in res):
                            res.append(c)
    return res


def build_frames(corners, tol):
    of = lambda t: [c for c in corners if c[0] == t]
    tls, trs, bls, brs = of("TL"), of("TR"), of("BL"), of("BR")
    frames, fails = [], []
    for tl in tls:
        x0, y0 = tl[1], tl[2]
        tr = bl = br = why = None
        for c in trs:
            if near(c[2], y0, tol) and c[1] > x0 + tol and (tr is None or c[1] < tr[1]):
                tr = c
        for c in bls:
            if near(c[1], x0, tol) and c[2] < y0 - tol and (bl is None or c[2] > bl[2]):
                bl = c
        if tr and bl:
            for c in brs:
                if near(c[1], tr[1], tol) and near(c[2], bl[2], tol):
                    br = c
        if tr:
            for c in tls:
                if near(c[2], y0, tol) and x0 + tol < c[1] < tr[1] - tol:
                    why = "TR missing?"
        if bl:
            for c in tls:
                if near(c[1], x0, tol) and bl[2] + tol < c[2] < y0 - tol:
                    why = "BL missing?"
        if tr is None:
            fails.append((x0, y0, "no TR"))
        elif bl is None:
            fails.append((x0, y0, "no BL"))
        elif br is None:
            fails.append((x0, y0, "no BR"))
        elif why:
            fails.append((x0, y0, why))
        else:
            frames.append((min(x0, bl[1]), min(bl[2], br[2]), max(tr[1], br[1]), max(y0, tr[2])))
    return frames, fails


def zorder(frames, tolrow):
    s = sorted(frames, key=lambda f: -f[3])  # 안정 정렬 (LISP 삽입정렬과 동일 성질)
    rows, row, refy = [], [], None
    for f in s:
        if row and near(f[3], refy, tolrow):
            row.append(f)
        else:
            if row:
                rows.append(row)
            row, refy = [f], f[3]
    if row:
        rows.append(row)
    out = []
    for r in rows:
        out += sorted(r, key=lambda f: f[0])
    return out


def mark_shape(e, tol):
    """LISP zf-mark-shape 와 동일. 맞으면 (선1, 선2, 짧은팔, 긴팔), 아니면 사유 문자열"""
    pts = e["pts"]
    if len(pts) != 3:
        return "점 개수 3개 아님"
    if e["closed"]:
        return "닫힌 선"
    if any(abs(b) > 1e-9 for b in e["bulges"]):
        return "곡선 구간 있음"
    ln1, ln2 = (pts[1], pts[0]), (pts[1], pts[2])
    h, v = split([ln1, ln2], tol)
    if len(h) == 1 and len(v) == 1:
        a, b = math.dist(pts[0], pts[1]), math.dist(pts[1], pts[2])
        return (ln1, ln2, min(a, b), max(a, b))
    return "가로+세로 모양 아님"


def get_marks(ents, samp, color, tol):
    """LISP zf-get-marks 와 동일. ssget 필터(점 3개, 색)를 먼저 적용"""
    lines, cnt, rej = [], 0, {}
    for e in ents:
        if len(e["pts"]) != 3 or (color is not None and e["color"] != color):
            continue
        cnt += 1
        r = mark_shape(e, tol)
        if isinstance(r, str):
            rej[r] = rej.get(r, 0) + 1
        elif not (near(r[2], samp[2], tol) and near(r[3], samp[3], tol)):
            rej["팔 길이 다름"] = rej.get("팔 길이 다름", 0) + 1
        else:
            lines += [r[0], r[1]]
    return lines, cnt, rej


def run_marks(ents, samp_ent, tol_pt=0.5, tol_row=5.0):
    samp = mark_shape(samp_ent, tol_pt)
    lines, cnt, rej = get_marks(ents, samp, samp_ent["color"], tol_pt)
    frames, fails, corners = run(lines, tol_pt, tol_row)
    return frames, fails, corners, cnt, rej


def run(lines, tol_pt=0.5, tol_row=5.0):
    h, v = split(lines, tol_pt)
    corners = find_corners(h, v, tol_pt)
    frames, fails = build_frames(corners, tol_pt)
    return zorder(frames, tol_row), fails, corners
