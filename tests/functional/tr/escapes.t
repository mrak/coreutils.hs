  $ export EXECUTABLE="tr"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FLAG=""

Just a backslack
  $ echo aba | $PROGRAM b '\'
  a\a

Just that character
  $ echo aba | $PROGRAM b '\c'
  aca

Octals
  $ echo 987 | $PROGRAM '\071' a
  a87
  $ echo 987 | $PROGRAM '\0717' ab
  a8b

More than one escape sequence
  $ echo aba | $PROGRAM ab '\f\t'
  \x0c\t\x0c (esc)
