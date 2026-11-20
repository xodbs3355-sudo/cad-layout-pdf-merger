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


def run(lines, tol_pt=0.5, tol_row=5.0):
    h, v = split(lines, tol_pt)
    corners = find_corners(h, v, tol_pt)
    frames, fails = build_frames(corners, tol_pt)
    return zorder(frames, tol_row), fails, corners
