  $ export EXECUTABLE="cat"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"

STDIN from a pipe
  $ printf "hello, world" | $PROGRAM
  hello, world (no-eol)

STDIN from -
  $ printf "hello, world" | $PROGRAM -
  hello, world (no-eol)
