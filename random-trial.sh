#! /bin/sh

#OPT="-4"

#trials=$1
#n=$2

trials=1
p=$1
x=$2

#ulimit -t 600

for n in $(seq 2000 20000); do
  p=$(echo "$n * l($n) / (($n * ($n-1)) / 2)" | bc -l)
  for i in $(seq 0 $((trials - 1))); do
    printf "%3d %4.2f %3d " $n $p $((x+i))
    (./random-graph $n $p $((x+i)) | ./ecc -s) 2>/dev/null || echo # $OPT
  done
done
