  $ export PROGRAM="${TESTDIR}/../../../$(stack path --dist-dir | cut -d' ' -f2)build/tr/tr"
  $ export FLAG=""

Just a backslack
  $ echo aba | $PROGRAM b '\'
  a\a

Just that character
  $ echo aba | $PROGRAM b '\c'
  aca

More than one escape sequence
  $ echo aba | $PROGRAM ab '\f\t'
  \x0c\t\x0c (esc)
