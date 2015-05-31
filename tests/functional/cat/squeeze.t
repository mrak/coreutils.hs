  $ export PROGRAM="${TESTDIR}/../../../dist/build/cat/cat"
  $ export FLAG="-s"

  $ printf "first\n\n\n\n\nlast" | $PROGRAM $FLAG
  first
   (re)
  last (no-eol)
