  $ export PROGRAM="${TESTDIR}/../../../$(stack path --dist-dir | cut -d' ' -f2)build/wc/wc"
  $ export FLAG="-w"
  $ printf "" | $PROGRAM $FLAG
  0
  $ printf "x" | $PROGRAM $FLAG
  1
  $ printf "x y\n" | $PROGRAM $FLAG
  2
  $ printf "x y\nz" | $PROGRAM $FLAG
  3
