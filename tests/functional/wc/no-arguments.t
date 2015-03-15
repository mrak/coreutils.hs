  $ export PROGRAM="${TESTDIR}/../../../dist/build/wc/wc"
  $ printf "" | $PROGRAM
        0       0       0
  $ printf "a b\nc\n" | $PROGRAM
        2       3       6
