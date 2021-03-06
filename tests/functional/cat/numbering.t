  $ export EXECUTABLE="cat"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FLAG="-n"

All lines
  $ printf "one\n\ntwo\nthree" | $PROGRAM -n
       1  one
       2   (re)
       3  two
       4  three (no-eol)

Non-blank lines
  $ printf "one\n\ntwo\nthree" | $PROGRAM -b
       1  one
   (re)
       2  two
       3  three (no-eol)

Applies numbering first
  $ printf "one\n\ntwo\nthree" | $PROGRAM -b -E
       1  one$
  $
       2  two$
       3  three$ (no-eol)
