  $ export EXECUTABLE="cat"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FLAG="-s"

  $ printf "first\n\n\n\n\nlast" | $PROGRAM $FLAG
  first
   (re)
  last (no-eol)
