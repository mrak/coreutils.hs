  $ export PROGRAM="${TESTDIR}/../../dist/build/wc/wc"
  $ export FLAG="--files0-from=-"
  $ export FILE="${CRAMTMP}/files0-from.t.input"

Stdin single file

  $ printf "line one
  > line two
  > line three" > "$FILE"
  $ printf "$FILE" \
  > | $PROGRAM $FLAG \
  > | sed -e "s|${CRAMTMP}|/tmp|g"
   2  6 28 /tmp/files0-from.t.input
  $ rm "$FILE"

Stdin multi file

  $ printf "line one
  > line two
  > line three" > "${FILE}1"
  $ printf "line one (again)
  > line two (again)
  > line three (again, and again, and again)" > "${FILE}2"
  $ printf "${FILE}1\000${FILE}2" \
  > | $PROGRAM $FLAG \
  > | sed -e "s|${CRAMTMP}|/tmp|g"
    2   6  28 /tmp/files0-from.t.input1
    2  13  74 /tmp/files0-from.t.input2
    4  19 102 total
  $ rm "${FILE}1" "${FILE}2"

From file

  $ export ARG="${CRAMTMP}/files0-from.t.arg"
  $ export FLAG="--files0-from=${ARG}"

Single

  $ printf "line one
  > line two
  > line three" > "$FILE"
  $ printf "$FILE" > "$ARG"
  $ $PROGRAM $FLAG | sed -e "s|${CRAMTMP}|/tmp|g"
   2  6 28 /tmp/files0-from.t.input
  $ rm "$FILE" "$ARG"

Multi

  $ printf "line one
  > line two
  > line three" > "${FILE}1"
  $ printf "line one (again)
  > line two (again)
  > line three (again, and again, and again)" > "${FILE}2"
  $ printf "${FILE}1\000${FILE}2" > "$ARG"
  $ $PROGRAM $FLAG | sed -e "s|${CRAMTMP}|/tmp|g"
    2   6  28 /tmp/files0-from.t.input1
    2  13  74 /tmp/files0-from.t.input2
    4  19 102 total
  $ rm "$ARG"
  $ rm "${FILE}1" "${FILE}2"
