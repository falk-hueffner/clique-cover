#! /bin/sh

OPT=""

echo -n "Dataset A " && ./ecc $OPT -s -c < test/dataset_a.graph
echo -n "Dataset B " && ./ecc $OPT -s -c < test/dataset_b.graph
echo -n "O       4 " && ./ecc $OPT -s -c < test/perf_match_4.graph
echo -n "Test    4 " && ./ecc $OPT -s < test/test4.graph
echo -n "Test    3 " && ./ecc $OPT -s < test/test3.graph
echo -n "Test    1 " && ./ecc $OPT -s < test/test1.graph
echo -n "O       5 " && ./ecc $OPT -s -c < test/perf_match_5.graph
echo -n "Test    2 " && ./ecc $OPT -s < test/test2.graph
echo -n "Dataset E " && ./ecc $OPT -s -c < test/dataset_e.graph
echo -n "Dataset D " && ./ecc $OPT -s -c < test/dataset_d.graph
echo -n "Dataset C " && ./ecc $OPT -s -c < test/dataset_c.graph
echo -n "Gnmp    2 " && ./ecc $OPT -s < test/gnmp02.graph
echo -n "Test    6 " && ./ecc $OPT -s < test/test6.graph
echo -n "Sparse  1 " && ./ecc $OPT -s < test/sparse01.graph
echo -n "O       6 " && ./ecc $OPT -s -c < test/perf_match_6.graph
echo -n "Random  1 " && ./ecc $OPT -s < test/random01.graph
echo -n "Random  2 " && ./ecc $OPT -s < test/random02.graph
echo -n "Gnmp    1 " && ./ecc $OPT -s < test/gnmp01.graph
echo -n "Test    5 " && ./ecc $OPT -s < test/test5.graph
