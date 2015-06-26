  $ export PROGRAM="${TESTDIR}/../../../$(stack path --dist-dir | cut -d' ' -f2)build/tr/tr"
  $ export FLAG=""

  $ echo 'abcde' | $PROGRAM $FLAG abcd xy
  xyyye
