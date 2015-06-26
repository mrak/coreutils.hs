  $ export PROGRAM="${TESTDIR}/../../../$(stack path --dist-dir | cut -d' ' -f2)build/wc/wc"
  $ export FLAG="-m"
  $ printf "üñiçōĐə" | $PROGRAM $FLAG
  7
