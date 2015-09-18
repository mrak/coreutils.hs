  $ export EXECUTABLE="true"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"

  $ [ 0 = 0 ] || true && echo "true"
  true

  $ $PROGRAM && echo "true"
  true
