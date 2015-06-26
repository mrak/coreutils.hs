  $ export PROGRAM="${TESTDIR}/../../../$(stack path --dist-dir | cut -d' ' -f2)build/wc/wc"
  $ printf "" | $PROGRAM
        0       0       0
  $ printf "a b\nc\n" | $PROGRAM
        2       3       6
