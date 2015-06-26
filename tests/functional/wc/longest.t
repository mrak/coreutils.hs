  $ export PROGRAM="${TESTDIR}/../../../$(stack path --dist-dir | cut -d' ' -f2)build/wc/wc"
  $ export FLAG="-L"
  $ printf "1\n12\n" | $PROGRAM $FLAG
  2
  $ printf "1\n123\n1\n" | $PROGRAM $FLAG
  3
  $ printf "\n123456" | $PROGRAM $FLAG
  6
  $ printf "üñiçōĐə" | $PROGRAM $FLAG
  7
