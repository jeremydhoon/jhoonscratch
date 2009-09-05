#!/usr/bin/python

def generate_trees(n,a,b,c,d,x,y,m):
    ret = set([(x,y)])
    for i in range(n-1):
        x = (a*x + b) % m
        y = (c*y + d) % m
        ret.add((x,y))
    #print "TREES:", ret
    return list(ret)

def load(fh):
    n = int(fh.readline())
    ret = []
    for line in fh.readlines():
        nums = map(int,line.split())
        assert len(nums) == 8
        ret.append(nums)
    assert len(ret) == n
    return ret

def handle_case(nums):
    trees = generate_trees(*nums)
    count = 0
    fail = 0
    for zerondx,(x0,y0) in enumerate(trees):
        for onendx,(x1,y1) in enumerate(trees[zerondx+1:]):
            for twondx, (x2,y2) in enumerate(trees[onendx+1:]):
                toadd = int(not bool((sum([x0,x1,x2]) % 3) + (sum([y0,y1,y2]) % 3)))
                count += toadd
                #fail += 1 - toadd
#    print "Count:",count," fail:", fail
    return count

def handle_cases(cases):
    for cndx,c in enumerate(cases):
        out = handle_case(c)
        print "Case #%d: %d" % (cndx+1,out)

EXAMPLE = \
"""2
4 10 7 1 2 0 1 20
6 2 0 2 1 1 2 11"""

if __name__ == '__main__':
    import sys
    import StringIO
    buf = StringIO.StringIO(EXAMPLE)
    infile = buf
    #infile = open(sys.argv[1])
    cases = load(infile)
    infile.close()
    handle_cases(cases)
