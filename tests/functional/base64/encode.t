  $ export EXECUTABLE="base64"
  $ export PROGRAM="${TESTDIR}/../../../$(cd $TESTDIR; stack path --dist-dir | cut -d' ' -f2)/build/${EXECUTABLE}/${EXECUTABLE}"

Encoding
  $ printf "yessiree" | $PROGRAM
  eWVzc2lyZWU=

  $ printf "two
  > lines" | $PROGRAM
  dHdvCmxpbmVz

Wrap default at 76
  $ printf "some very long string that is longer that 76 characters to see if base64 wraps the output" | $PROGRAM
  c29tZSB2ZXJ5IGxvbmcgc3RyaW5nIHRoYXQgaXMgbG9uZ2VyIHRoYXQgNzYgY2hhcmFjdGVycyB0
  byBzZWUgaWYgYmFzZTY0IHdyYXBzIHRoZSBvdXRwdXQ=

Wrapping at a given point
  $ printf "some very long string that is longer that 76 characters to see if base64 wraps the output" | $PROGRAM -w 50
  c29tZSB2ZXJ5IGxvbmcgc3RyaW5nIHRoYXQgaXMgbG9uZ2VyIH
  RoYXQgNzYgY2hhcmFjdGVycyB0byBzZWUgaWYgYmFzZTY0IHdy
  YXBzIHRoZSBvdXRwdXQ=

Disable wrapping
  $ printf "some very long string that is longer that 76 characters to see if base64 wraps the output" | $PROGRAM -w 0
  c29tZSB2ZXJ5IGxvbmcgc3RyaW5nIHRoYXQgaXMgbG9uZ2VyIHRoYXQgNzYgY2hhcmFjdGVycyB0byBzZWUgaWYgYmFzZTY0IHdyYXBzIHRoZSBvdXRwdXQ=

Decoding
  $ printf "eWVzc2lyZWU=" | $PROGRAM -d
  yessiree

  $ printf "eWV\033zc2lyZWU=" | $PROGRAM -d -i
  yessiree

  $ printf "
  > c29tZSB2ZXJ5IGxvbmcgc3RyaW5nIHRoYXQgaXMgbG9uZ2VyIH
  > RoYXQgNzYgY2hhcmFjdGVycyB0byBzZWUgaWYgYmFzZTY0IHdy
  > YXBzIHRoZSBvdXRwdXQ=" | $PROGRAM -d
  some very long string that is longer that 76 characters to see if base64 wraps the output
