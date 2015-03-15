  $ export PROGRAM="${TESTDIR}/../../../dist/build/wc/wc"
  $ export FLAG="-c"
  $ printf "" | $PROGRAM $FLAG
  0
  $ printf "x" | $PROGRAM $FLAG
  1
