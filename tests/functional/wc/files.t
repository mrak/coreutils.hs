  $ export EXECUTABLE="wc"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FLAG=""

Single file

  $ printf "line one
  > line two
  > line three" > "$CRAMTMP"/files.t.input
  $ $PROGRAM $FLAG "$CRAMTMP"/files.t.input | sed -e "s|${CRAMTMP}|/tmp|g"
   2  6 28 /tmp/files.t.input
  $ rm "$CRAMTMP"/files.t.input

Multiple files

  $ printf "line one
  > line two
  > line three" > "$CRAMTMP"/files.t.input1
  $ printf "line one (again)
  > line two (again)
  > line three (again, and again, and again)" > "$CRAMTMP"/files.t.input2
  $ $PROGRAM $FLAG "$CRAMTMP"/files.t.input* | sed -e "s|${CRAMTMP}|/tmp|g"
    2   6  28 /tmp/files.t.input1
    2  13  74 /tmp/files.t.input2
    4  19 102 total
