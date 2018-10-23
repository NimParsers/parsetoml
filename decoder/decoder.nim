# https://github.com/BurntSushi/toml-test#try-it-out
# 1. Install the toml-test using:
#      go get github.com/BurntSushi/toml-test
# 2. Build this parsetoml based decoder:
#      nim c -d:release decoder.nim
# 3. Test it
#      toml-test decoder

import json, streams, os
import parsetoml

var
  fs: FileStream

if paramCount() > 0:
  fs = newFileStream(paramStr(1), bufSize = 32768)
else:
  fs = newFileStream(stdin)

if fs.isNil:
  echo "Error: '", paramStr(1), "' can't be opened!"
  quit(QuitFailure)

let table = parsetoml.parseStream(fs)

fs.close

echo table.toJson.pretty
