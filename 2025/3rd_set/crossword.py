import sys
from collections import defaultdict

def load(p):
    with open(p) as f:
        lines = [l.strip() for l in f if l.strip()]
    R, C, B, W = map(int, lines[0].split())
    blacks = [tuple(int(x) - 1 for x in lines[i + 1].split()) for i in range(B)]
    words = lines[1 + B:]
    return R, C, blacks, words

def slots(R, C, blk):
    g = [['.' for _ in range(C)] for _ in range(R)]
    for r, c in blk:
        g[r][c] = '#'
    s = []
    for r in range(R):
        c = 0
        while c < C:
            if g[r][c] == '#':
                c += 1
                continue
            st = c
            while c < C and g[r][c] != '#':
                c += 1
            if c - st >= 2:
                s.append([(r, i) for i in range(st, c)])
    for c in range(C):
        r = 0
        while r < R:
            if g[r][c] == '#':
                r += 1
                continue
            st = r
            while r < R and g[r][c] != '#':
                r += 1
            if r - st >= 2:
                s.append([(i, c) for i in range(st, r)])
    return g, s

def solve(R, C, blk, wds):
    g, s = slots(R, C, blk)
    by_len = defaultdict(list)
    for i, w in enumerate(wds):
        by_len[len(w)].append(i)
    used = [False] * len(wds)
    ass = [None] * len(s)

    def fits(i, w):
        return all(g[r][c] in ('.', ch) for (r, c), ch in zip(s[i], w))

    def place(i, w):
        m = []
        for (r, c), ch in zip(s[i], w):
            if g[r][c] == '.':
                g[r][c] = ch
                m.append((r, c))
        return m

    def undo(m):
        for r, c in m:
            g[r][c] = '.'

    def choose():
        best = None
        bc = None
        bestn = 10**9
        for i, cells in enumerate(s):
            if ass[i] is not None:
                continue
            L = len(cells)
            cand = [idx for idx in by_len[L] if not used[idx] and fits(i, wds[idx])]
            if not cand:
                return None, []
            if len(cand) < bestn:
                bestn, best, bc = len(cand), i, cand
                if bestn == 1:
                    break
        return best, bc

    def dfs(f):
        if f == len(s):
            return True
        i, cand = choose()
        if i is None:
            return False
        for idx in cand:
            mod = place(i, wds[idx])
            ass[i] = idx
            used[idx] = True
            if dfs(f + 1):
                return True
            used[idx] = False
            ass[i] = None
            undo(mod)
        return False

    if not dfs(0):
        return None
    out = []
    for r in range(R):
        row = []
        c = 0
        while c < C:
            if g[r][c] == '#':
                c += 1
                continue
            buf = []
            while c < C and g[r][c] != '#':
                buf.append(g[r][c])
                c += 1
            if len(buf) >= 2:
                row.append(''.join(buf))
        out.append(' '.join(row))
    return out

R, C, blk, wds = load(sys.argv[1])
res = solve(R, C, blk, wds)
if res is None:
    print("IMPOSSIBLE")
else:
    for l in res:
        print(l)
