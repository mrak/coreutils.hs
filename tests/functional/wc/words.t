  $ export EXECUTABLE="wc"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FLAG="-w"
  $ printf "" | $PROGRAM $FLAG
  0
  $ printf "x" | $PROGRAM $FLAG
  1
  $ printf "x y\n" | $PROGRAM $FLAG
  2
  $ printf "x y\nz" | $PROGRAM $FLAG
  3
