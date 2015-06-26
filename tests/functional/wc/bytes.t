  $ export PROGRAM="${TESTDIR}/../../../$(stack path --dist-dir | cut -d' ' -f2)build/wc/wc"
  $ export FLAG="-c"
  $ printf "" | $PROGRAM $FLAG
  0
  $ printf "x" | $PROGRAM $FLAG
  1
