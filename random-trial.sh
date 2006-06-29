#! /bin/sh

OPT="-4"

trials=$1
n=$2

if [ -n "$3" ]; then
  p=$3
else
  p=$(echo "$n * l($n) / (($n * ($n-1)) / 2)" | bc -l)
fi

(for i in $(seq $trials); do
  ./random-graph $n $p $i | ./ecc -s $OPT | tee /dev/stderr
done) \
| ./avg.py
