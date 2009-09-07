def parsemaps(fname):
    infile = open(fname)
    nmaps = int(infile.readline())
    maps = []
    for i in range(nmaps):
        h,w = map(int, infile.readline().split())
        maplist = []
        for j in range(h):
            row = map(int, infile.readline().split())
            assert len(row) == w
            maplist.append(row)
        assert len(maplist) == h
        maps.append(maplist)
    assert len(maps) == nmaps
    return maps

def findbasins(maplist):
    nrows = len(maplist)
    ncols = len(maplist[0])
    names = [chr(c) for c in range(ord('a'), ord('a') + 27)]
    name = names[0]
    def nextname(n):
        return names[names.index(n) + 1]
    def get_neighbors(rndx,cndx):
        if rndx != 0:
            yield (rndx-1,cndx)
        if cndx != 0:
            yield (rndx, cndx-1)
        if cndx < ncols - 1:
            yield (rndx, cndx + 1)
        if rndx < nrows - 1:
            yield (rndx + 1, cndx)
    def lowest_neighbor(rndx,cndx):
        height = maplist[rndx][cndx]
        currbest = height
        currcell = None
        for nbor_r,nbor_c in get_neighbors(rndx,cndx):
            score = maplist[nbor_r][nbor_c]
            if score < currbest:
                currbest = score
                currcell = (nbor_r, nbor_c)
            #if nbor_r == rndx+1 and nbor_c == cndx:
            #    print "went south"
        return currcell
    labels = [[None for col in row] for row in maplist]
    for rndx,row in enumerate(maplist):
        for cndx,col in enumerate(row):
            #print "NEW LABELS"
            #for lblrow in labels:
            #    for lblcol in lblrow:
            #        if lblcol is None:
            #            print ' ',
            #        else:
            #            print lblcol,
            #    print ''
            if labels[rndx][cndx] is None:
                trail = [(rndx,cndx)]
                curr = lowest_neighbor(rndx,cndx)
                basin = None
                while curr is not None:
                    currlabel = labels[curr[0]][curr[1]]
                    if currlabel is not None:
                        basin = currlabel
                        break
                    else:
                        trail.append(curr)
                        curr = lowest_neighbor(curr[0], curr[1])
                if not basin:
                    basin = name
                    name = nextname(name)
                #print trail, name
                for lblr,lblc in trail:
                    labels[lblr][lblc] = basin
                
    return labels

if __name__ == '__main__':
    import glob
    fname = glob.glob("B-large*.in")[0]
    maps = parsemaps(fname)
    #maps = [maps[-3]] # remove me!!!!
    outfile = open("b-large.out", "w")
    for ndx,m in enumerate(maps):
        lbls = findbasins(m)
        outfile.write("Case #%d:" % (ndx+1))
        outfile.write("\n")
        joinedlines = [' '.join(line) for line in lbls]
        outfile.write('\n'.join(joinedlines))
        outfile.write('\n')
    outfile.close()
