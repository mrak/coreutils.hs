  $ export EXECUTABLE="tr"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FLAG=""

  $ echo 'abcde' | $PROGRAM $FLAG abcd xy
  xyyye
