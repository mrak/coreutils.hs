  $ export EXECUTABLE="head"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FILE="$CRAMTMP"/head-bytes.txt

  $ printf "line one
  > line two
  > line three
  > line four
  > line five
  > line six
  > line seven
  > line eight
  > line nine
  > line ten
  > line eleven
  > line twelve" > "$FILE"

Positive
  $ $PROGRAM -c4 "$FILE"
  line (no-eol)

Negative
  $ $PROGRAM -c-4 "$FILE"
  line one
  line two
  line three
  line four
  line five
  line six
  line seven
  line eight
  line nine
  line ten
  line eleven
  line tw (no-eol)

  $ rm "$FILE"
