Single file

  $ export PROGRAM="${TESTDIR}/../../dist/build/wc/wc"
  $ export FLAG=""
  $ printf "line one
  > line two
  > line three" > "$CRAMTMP"/files.t.input
  $ $PROGRAM $FLAG "$CRAMTMP"/files.t.input | sed -e "s|${CRAMTMP}|/tmp|g"
   2  6 28 /tmp/files.t.input
  $ rm "$CRAMTMP"/files.t.input

Multiple files

  $ export PROGRAM="${TESTDIR}/../../dist/build/wc/wc"
  $ export FLAG=""
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