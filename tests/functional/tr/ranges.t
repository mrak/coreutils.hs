  $ export EXECUTABLE="tr"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FLAG=""

  $ echo 'abcdefghijklmnopqrstuvwxyz' | $PROGRAM a-e v-z
  vwxyzfghijklmnopqrstuvwxyz

Hyphen literal
  $ echo '-abcdefghijklmnopqrstuvwxyz' | $PROGRAM 'a\-e' 'xyz'
  yxbcdzfghijklmnopqrstuvwxyz
  $ echo "xy-z" | tr '\r\-\141' gbc
  xybz
  $ echo "xy-z" | tr '\r-\141' gbc
  xycz
