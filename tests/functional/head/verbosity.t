  $ export EXECUTABLE="head"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FILE1="$CRAMTMP"/head-verbosity1.txt
  $ export FILE2="$CRAMTMP"/head-verbosity2.txt

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
  > line twelve" > "$FILE1"
  $ printf "row one
  > row two
  > row three
  > row four
  > row five
  > row six
  > row seven
  > row eight
  > row nine
  > row ten
  > row eleven
  > row twelve" > "$FILE2"

Default verbosity
  $ $PROGRAM "$FILE1"
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

  $ $PROGRAM "$FILE1" "$FILE2" | sed -e "s|${CRAMTMP}|/tmp|g"
  ==> /tmp/head-verbosity1.txt <==
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
   (glob)
  ==> /tmp/head-verbosity2.txt <==
  row one
  row two
  row three
  row four
  row five
  row six
  row seven
  row eight
  row nine
  row ten

  $ rm "$FILE1" "$FILE2"
