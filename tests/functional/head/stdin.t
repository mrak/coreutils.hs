  $ export EXECUTABLE="head"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"
  $ export FILE="$CRAMTMP"/head-bytes.txt

  $ printf "line one
  > line two" | $PROGRAM -n1
  line one
