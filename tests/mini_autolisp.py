"""시험 전용 초소형 AutoLISP 해석기.

zframe.lsp 의 '계산 함수'(zf-split, zf-find-corners, zf-build-frames, zf-zorder 등)를
AutoCAD 없이 실제 LISP 코드 그대로 실행해 보기 위한 것.
- 지원: 이 파일이 쓰는 기본 함수 일부만. AutoCAD 함수(entget, ssget, entmake 등)는 없음.
- AutoCAD의 실제 AutoLISP와 세부 동작이 다를 수 있음(예: 정수/실수 나눗셈, 오류 메시지).
"""
import math

NIL = None


class Sym(str):
    pass


class Dotted(tuple):  # (a . b)
    pass


def tokenize(src):
    toks, i, n = [], 0, len(src)
    while i < n:
        c = src[i]
        if c.isspace():
            i += 1
        elif c == ";":
            while i < n and src[i] != "\n":
                i += 1
        elif c in "()'":
            toks.append(c)
            i += 1
        elif c == '"':
            j, buf = i + 1, []
            while src[j] != '"':
                if src[j] == "\\":
                    j += 1
                    buf.append({"n": "\n", "t": "\t"}.get(src[j], src[j]))
                else:
                    buf.append(src[j])
                j += 1
            toks.append(("STR", "".join(buf)))
            i = j + 1
        else:
            j = i
            while j < n and not src[j].isspace() and src[j] not in "()';\"":
                j += 1
            toks.append(src[i:j])
            i = j
    return toks


def atom(t):
    if isinstance(t, tuple):
        return t[1]
    try:
        return int(t)
    except ValueError:
        pass
    try:
        return float(t)
    except ValueError:
        return Sym(t.upper())


def parse(toks):
    pos = 0

    def read():
        nonlocal pos
        t = toks[pos]
        pos += 1
        if t == "'":
            return [Sym("QUOTE"), read()]
        if t == "(":
            out = []
            while toks[pos] != ")":
                if toks[pos] == ".":
                    pos += 1
                    tail = read()
                    out = Dotted((out[0], tail))
                    continue
                out.append(read())
            pos += 1
            return out
        return atom(t)

    forms = []
    while pos < len(toks):
        forms.append(read())
    return forms


def truthy(v):
    return not (v is None or v == [])


def L(v):  # 파이썬 list -> LISP 값 (빈 리스트는 nil)
    return v if v else NIL


class Lambda:
    def __init__(self, params, locs, body, interp):
        self.params, self.locs, self.body, self.interp = params, locs, body, interp

    def __call__(self, *args):
        g = self.interp.g
        names = self.params + self.locs
        saved = {k: g.get(k, NIL) for k in names}  # 동적 범위(dynamic scope) 흉내
        for k in self.locs:
            g[k] = NIL
        for k, a in zip(self.params, args):
            g[k] = a
        try:
            r = NIL
            for f in self.body:
                r = self.interp.ev(f)
            return r
        finally:
            g.update(saved)


def lst(v):
    return [] if v is None else v


def lisp_type(v):
    if v is None:
        return NIL
    if isinstance(v, str) and not isinstance(v, Sym):
        return Sym("STR")
    if isinstance(v, bool):
        return Sym("SYM")
    if isinstance(v, int):
        return Sym("INT")
    if isinstance(v, float):
        return Sym("REAL")
    if isinstance(v, (list, tuple)):
        return Sym("LIST")
    return Sym("USUBR")


class Interp:
    def __init__(self):
        self.g = {}
        num = lambda x: x
        B = {
            "+": lambda *a: sum(a), "*": lambda *a: math.prod(a),
            "-": lambda a, *b: a - sum(b) if b else -a,
            "/": lambda a, b: (a // b if isinstance(a, int) and isinstance(b, int) else a / b),
            "<": lambda a, b: a < b, ">": lambda a, b: a > b,
            "<=": lambda a, b: a <= b, ">=": lambda a, b: a >= b,
            "=": lambda a, b: a == b, "/=": lambda a, b: a != b,
            "1+": lambda a: a + 1, "1-": lambda a: a - 1, "ABS": abs, "MIN": min, "MAX": max,
            "CAR": lambda x: lst(x)[0] if x else NIL,
            "CDR": lambda x: (x[1] if isinstance(x, Dotted) else L(lst(x)[1:])),
            "CADR": lambda x: lst(x)[1] if len(lst(x)) > 1 else NIL,
            "CADDR": lambda x: lst(x)[2] if len(lst(x)) > 2 else NIL,
            "NTH": lambda i, x: lst(x)[i] if i < len(lst(x)) else NIL,
            "LENGTH": lambda x: len(lst(x)),
            "CONS": lambda a, b: ([a] + lst(b)) if (b is None or isinstance(b, list)) and not isinstance(b, Dotted) else Dotted((a, b)),
            "ASSOC": lambda k, al: next((e for e in lst(al) if e[0] == k), NIL),
            "SUBST": lambda new, old, al: L([new if e == old else e for e in lst(al)]),
            "LOGAND": lambda *a: __import__("functools").reduce(lambda x, y: x & y, a),
            "TYPE": lisp_type,
            "LIST": lambda *a: L(list(a)),
            "APPEND": lambda *a: L([e for x in a for e in lst(x)]),
            "REVERSE": lambda x: L(lst(x)[::-1]),
            "NULL": lambda x: not truthy(x), "NOT": lambda x: not truthy(x),
            "DISTANCE": lambda p, q: math.dist(p[:2], q[:2]) if len(p) == len(q) else math.dist(p[:2], q[:2]),
            "STRCAT": lambda *a: "".join(a), "ITOA": lambda i: str(i),
            "STRLEN": lambda s: len(s),
            "SUBSTR": lambda s, start, n=None: s[start - 1:] if n is None else s[start - 1:start - 1 + n],
            "RTOS": lambda x, m=2, p=4: f"{x:.{p}f}",
            "APPLY": lambda f, args: f(*lst(args)),
            "PRINC": lambda *a: NIL,
        }
        self.g.update({Sym(k): v for k, v in B.items()})
        self.g[Sym("T")] = True
        self.g[Sym("NIL")] = NIL

    def load(self, src):
        for f in parse(tokenize(src)):
            self.ev(f)

    def mk_lambda(self, plist):
        params, locs, cur = [], [], None
        cur = params
        for p in lst(plist):
            if p == "/":
                cur = locs
            else:
                cur.append(p)
        return params, locs

    def ev(self, x):
        if isinstance(x, Sym):
            return self.g.get(x, NIL)
        if not isinstance(x, list) or isinstance(x, Dotted):
            return x
        if not x:
            return NIL
        op = x[0]
        if op == "QUOTE":
            q = x[1]
            return L(q) if isinstance(q, list) and not isinstance(q, Dotted) else q
        if op == "SETQ":
            r = NIL
            for k, v in zip(x[1::2], x[2::2]):
                r = self.g[k] = self.ev(v)
            return r
        if op == "DEFUN":
            p, l = self.mk_lambda(x[2])
            self.g[x[1]] = Lambda(p, l, x[3:], self)
            return x[1]
        if op in ("LAMBDA",):
            p, l = self.mk_lambda(x[1])
            return Lambda(p, l, x[2:], self)
        if op == "FUNCTION":
            return self.ev(x[1])
        if op == "IF":
            if truthy(self.ev(x[1])):
                return self.ev(x[2])
            return self.ev(x[3]) if len(x) > 3 else NIL
        if op == "COND":
            for cl in x[1:]:
                v = self.ev(cl[0])
                if truthy(v):
                    for f in cl[1:]:
                        v = self.ev(f)
                    return v
            return NIL
        if op == "PROGN":
            r = NIL
            for f in x[1:]:
                r = self.ev(f)
            return r
        if op == "AND":
            for f in x[1:]:
                if not truthy(self.ev(f)):
                    return NIL
            return True
        if op == "OR":
            for f in x[1:]:
                if truthy(self.ev(f)):
                    return True
            return NIL
        if op == "WHILE":
            while truthy(self.ev(x[1])):
                for f in x[2:]:
                    self.ev(f)
            return NIL
        if op == "FOREACH":
            var, saved, r = x[1], self.g.get(x[1], NIL), NIL
            for e in lst(self.ev(x[2])):
                self.g[var] = e
                for f in x[3:]:
                    r = self.ev(f)
            self.g[var] = saved
            return r
        fn = self.ev(op)
        if fn is NIL:
            raise NameError(f"no function: {op}")
        r = fn(*[self.ev(a) for a in x[1:]])
        if r is False:
            return NIL
        if isinstance(r, list) and not r:
            return NIL
        return r
