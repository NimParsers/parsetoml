import parsetoml
import json
import streams

let table = parsetoml.parseStream(newFileStream(stdin))

echo table.toJson.pretty
