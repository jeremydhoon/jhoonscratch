def parsecase(case):
    case = case.strip()
    if not '(' in case:
        return [case]
    ret = []
    chunk = []
    choosing = False
    for c in case:
        if c == '(' and not choosing:
            if len(chunk):
                ret.append(''.join(chunk))
            chunk = []
            choosing = True
        elif c == '(' and not choosing:
            raise ValueError, "Unexpected open paren"
        elif c == ')' and choosing:
            ret.append(chunk)
            chunk = []
            choosing = False
        elif c == ')' and not choosing:
            raise ValueError, "Unexpected close paren"
        else:
            chunk.append(c)
    end = ''.join(chunk)
    if len(end):
        ret.append(end)
    return ret

def words_by_length(words):
    ret = []
    maxlen = max(map(len, words))
    for i in range(1,maxlen+1):
        lenarr = []
        for w in words:
            lenarr.append(w[:i])
        ret.append(set(lenarr))
    return ret

def parseldn(fname):
    infile = open(fname)
    lines = infile.readlines()
    infile.close()
    word_length, n_words, n_cases = map(int, lines[0].split())
    words = set([l.strip() for l in lines[1:1+n_words]])
    test_cases = map(parsecase, lines[1+n_words:])
    assert len(test_cases) == n_cases
    return words, test_cases

def nmatched(case, words, lenwords):
    possible = []
    def helper(prefix, rest, depth):
        if (depth > 0 and prefix not in lenwords[depth-1]):
            return
        ret = []
        if len(rest) == 0:
            possible.append(prefix)
        else:   
            next = rest[0]
            if isinstance(next,str):
                helper(prefix + next, rest[1:], depth + len(next))
            else:
                for ele in rest[0]:
                    helper(prefix + ele, rest[1:], depth+1)
    helper('', case, 0)
    return len([word for word in possible if word in words])

if __name__ == '__main__':
    import glob
    #fname = "test.txt"
    fname = glob.glob("A-large*.in")[0]
    words, test_cases = parseldn(fname)
    lenwords = words_by_length(words)
    for ndx, case in enumerate(test_cases):
        print "Case #%d: %d" % (ndx+1, nmatched(case,words,lenwords))
