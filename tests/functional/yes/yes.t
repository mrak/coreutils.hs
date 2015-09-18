  $ export EXECUTABLE="yes"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"

  $ $PROGRAM | head -n2
  y
  y

  $ $PROGRAM why not? | head
  why not?
  why not?
  why not?
  why not?
  why not?
  why not?
  why not?
  why not?
  why not?
  why not?
