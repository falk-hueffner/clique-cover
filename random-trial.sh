#! /bin/sh

(for i in $(seq $1); do
  ./random-graph $2 $3 | ./ecc -s | tee /dev/stderr
done) \
| ./avg.py
