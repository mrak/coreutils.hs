  $ export EXECUTABLE="wc"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ printf "" | $PROGRAM
        0       0       0
  $ printf "a b\nc\n" | $PROGRAM
        2       3       6
