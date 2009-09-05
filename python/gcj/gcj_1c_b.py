#!/usr/bin/python

import random

def fastexp(n,k,m):
    mask = k
    ret = 1
    mult = n
    while mask:
        if mask & 0x1:
            ret *= mult
            ret %= m
        mult = mult*mult
        mult %= m
        mask >>= 1
    return ret

print fastexp(2,10,100)

def rabin_miller(n):
    if n == 2:
        return True
    elif  n%2 == 0:
        return False
    d_target = n - 1
    mask = 0x1
    s = 0
    for i in range(0,32):
        if not d_target % mask:
            s += i
            d_target /= mask
        mask <<= 1
    div = 1 << s
    d = (n-1)/div
    assert (fastexp(2,s,10000000000000)*d) == n - 1
    nm1 = n - 1
    for a in [2,3,5,7,11,13,17]:
        if a > n - 2:
            return True
        x = fastexp(a,d,n)
        if x == 1 or x == nm1:
            continue
        equal = False
        for r in range(1,s):
            x = fastexp(x,2,n)
            if x == 1:
                return False
            if x == nm1:
                equal = True
                break
        if not equal:
            return False
    return True

def compute_primes(start,end):
    ret = []
    for i in range(start, end+1):
        if rabin_miller(i):
            ret.append(i)
    return ret

def load(fh):
    c = int(fh.readline())
    ret = []
    for line in fh.readlines():
        toapp = map(int, line.split())
        assert len(toapp) == 3
        ret.append(toapp)
    assert len(ret) == c
    return ret

class NumSet(object):
    def __init__(self,index):
        self.parent = None
        self.rank = 1
        self.index = index
    @classmethod
    def climb(cls, ns):
        top = self
        while top.parent is not None:
            top = top.parent
        return top
    def merge(self, child):
        top = self
        while top.parent is not None:
            top = 
        

def handle_case(a,b,p):
    primes = compute_primes(p,b+1)
    primetable = []
    for x in range(a,b+1):
        arr = []
        for p in primes:
            if x%p == 0:
                arr.append(p)
        primetable.append(set(arr))
    setlist = [NumSet(x) for x in range(len(primetable))]
    for x in range(len(primetable)):
        px = primetable[x]
        if not len(px):
            continue
        for y in range(x+1,len(primetable)):
            py = primetable[y]
            if not len(py):
                 continue
            if len(px ^ py) < len(px) + len(py):
                setlist[x].merge(setlist[y])
                if y == 8 or y == 10:
                    print "MERGING",x,y
                    print setlist[x].index
                    print setlist[y].index
    print [x.index for x in setlist]
    print [x.parent.index if x.parent is not None else None for x in setlist]
    print [x.rank for x in setlist]
    return reduce(lambda acc,x: acc + int(x.parent is None), setlist, 0)
#    print sets
#    return len(set(sets))

def handle_cases(cases):
    for cndx,c in enumerate(cases):
        out = handle_case(*c)
        print "Case #%d: %d" % (cndx+1, out)

EXAMPLE =\
"""2
10 20 5
10 20 3"""

if __name__ == '__main__':
    import StringIO
    buf = StringIO.StringIO(EXAMPLE)
    infile = buf
    #infile = sys.argv[1]
    cases = load(infile)
    infile.close()
    handle_cases(cases)
