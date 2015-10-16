  $ export EXECUTABLE="head"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FILE="$CRAMTMP"/head-lines.txt

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

Defaults
  $ $PROGRAM "$FILE"
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

Positive
  $ $PROGRAM -n2 "$FILE"
  line one
  line two

Negative
  $ $PROGRAM -n-3 $FILE
  line one
  line two
  line three
  line four
  line five
  line six
  line seven
  line eight
  line nine

  $ rm "$FILE"
