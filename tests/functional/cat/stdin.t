  $ export PROGRAM="${TESTDIR}/../../../dist/build/cat/cat"

STDIN from a pipe
  $ printf "hello, world" | $PROGRAM
  hello, world (no-eol)

STDIN from -
  $ printf "hello, world" | $PROGRAM -
  hello, world (no-eol)
