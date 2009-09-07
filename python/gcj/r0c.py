def parselines(fname):
    infile = open(fname)
    nlines = int(infile.readline())
    lines = [l.strip() for l in infile.readlines()]
    infile.close()
    return lines

def count_phrase(line, phrase="welcome to code jam"):
    if len(line) < len(phrase):
        return 0
    line = '_' + line
    MOD = 16383
    symbols = set(line)
    occurs = {}
    dp = [[None for i in line] for j in phrase]
    # first initialize the top row
    seen = 0
    for ndx,c in enumerate(line):
        seen += int(c == phrase[0])
        dp[0][ndx] = seen
    for pndx,target in enumerate(phrase):
        if pndx == 0:
            continue
        dp[pndx][0] = 0
        prevseen_ndx = 0
        for i in range(pndx):
            dp[pndx][i] = 0
        for ndx,c in enumerate(line[pndx:]):
            ndx += pndx
            if ndx == 0:
                continue
            if c == target:
                dp[pndx][ndx] = (dp[pndx-1][ndx] + dp[pndx][ndx-1]) % MOD
            else:
                dp[pndx][ndx] = dp[pndx][ndx-1]
    #for line in dp:
    #    print ' '.join(map(str,line))
    return dp[-1][-1]


if __name__ == '__main__':
    import glob
    fname = glob.glob("C-large*.in")[0]
    lines = parselines(fname)
    for ndx,l in enumerate(lines):
        out = ("%04d" % count_phrase(l))[-4:]
        print "Case #%d: %s" % (ndx+1, out)
