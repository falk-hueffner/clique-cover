#! /usr/bin/env python

import sys, string, math

lines = []
while 1:
    line = sys.stdin.readline()
    if not line:
        break
    inputline = line
    lines.append(string.split(line))

n = len(lines)
k = len(lines[0])
for line in lines:
    if len(line) != k:
        raise "invalid number of columns"

avgs = []
for c in range(0, k):
    sum = 0.0
    for line in lines:
        sum += float(line[c])
    avgs.append(sum / n)

ds = []
for c in range(0, k):
    s = 0.0
    for line in lines:
        s += (float(line[c]) - avgs[c]) ** 2
    ds.append(math.sqrt(s / n))


WS   = 0
NOWS = 1
lastbreak = -1
i = 0
state = WS
cols = []
while True:
    if i >= len(inputline) or inputline[i].isspace():
        newstate = WS
    else:
        newstate = NOWS
    if state == NOWS and newstate == WS:
        cols.append(i - lastbreak - 1)
        lastbreak = i
    state = newstate        
    if i >= len(inputline):
        break
    i += 1

for c in range(0, k):
    if avgs[c] == 0:
        d = 0
    else:
        d = (ds[c] / avgs[c]) * 100
    if len(sys.argv) > 1:
        print "%s (%2.1f%%)" % (avgs[c], d),
        
    else:
        #print "%f" % (avgs[c]),
        if avgs[c] > 100:
            digits = 0
        elif avgs[c] > 10:
            digits = 1
        else:
            digits = 2
            
        print ("%" + `cols[c]` + "." + `digits` + "f") % avgs[c],
