  $ export PROGRAM="${TESTDIR}/../../../$(stack path --dist-dir | cut -d' ' -f2)build/cat/cat"
  $ export FLAG="-s"

  $ printf "first\n\n\n\n\nlast" | $PROGRAM $FLAG
  first
   (re)
  last (no-eol)
