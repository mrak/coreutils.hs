  $ export PROGRAM="${TESTDIR}/../../../$(stack path --dist-dir | cut -d' ' -f2)build/cat/cat"

STDIN from a pipe
  $ printf "hello, world" | $PROGRAM
  hello, world (no-eol)

STDIN from -
  $ printf "hello, world" | $PROGRAM -
  hello, world (no-eol)
