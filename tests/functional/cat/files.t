  $ export PROGRAM="${TESTDIR}/../../../dist/build/cat/cat"
  $ export FILE1="$CRAMTMP"/files.t.input1
  $ export FILE2="$CRAMTMP"/files.t.input2

  $ echo "line one
  > line two
  > line three" > $FILE1
  $ echo "line one (again)
  > line two (again)
  > line three (again, and again, and again)" > $FILE2

  $ $PROGRAM $FILE1 $FILE2
  line one
  line two
  line three
  line one (again)
  line two (again)
  line three (again, and again, and again)
