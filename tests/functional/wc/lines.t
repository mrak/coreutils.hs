Remember, -l counts *newline* bytes, not logical lines.

  $ export PROGRAM="${TESTDIR}/../../../$(stack path --dist-dir | cut -d' ' -f2)build/wc/wc"
  $ export FLAG="-l"
  $ printf "" | $PROGRAM $FLAG
  0
  $ printf "x y" | $PROGRAM $FLAG
  0
  $ printf "x y\n" | $PROGRAM $FLAG
  1
  $ printf "x\ny\n" | $PROGRAM $FLAG
  2
