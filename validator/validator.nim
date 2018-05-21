import parsetoml
import json
import streams
import os

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

