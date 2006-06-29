#! /bin/sh

OPT="-3 -4"

echo -n "Dataset A " && ./ecc $OPT -s -c < test/dataset_a.graph
echo -n "Dataset B " && ./ecc $OPT -s -c < test/dataset_b.graph
echo -n "O       4 " && ./ecc $OPT -s -c < test/perf_match_4.graph
echo -n "Test    4 " && ./ecc $OPT -s < test/test4.graph
echo -n "O       5 " && ./ecc $OPT -s -c < test/perf_match_5.graph
echo -n "Dataset E " && ./ecc $OPT -s -c < test/dataset_e.graph
echo -n "Test    3 " && ./ecc $OPT -s < test/test3.graph
echo -n "Test    6 " && ./ecc $OPT -s < test/test6.graph
echo -n "Dataset D " && ./ecc $OPT -s -c < test/dataset_d.graph
echo -n "Dataset C " && ./ecc $OPT -s -c < test/dataset_c.graph
echo -n "Test    1 " && ./ecc $OPT -s < test/test1.graph
echo -n "Test    5 " && ./ecc $OPT -s < test/test5.graph
echo -n "O       6 " && ./ecc $OPT -s -c < test/perf_match_6.graph
echo -n "Test    2 " && ./ecc $OPT -s < test/test2.graph
