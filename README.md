# NimToml

Parsetoml is a Nim library to parse TOML files
(https://github.com/toml-lang/toml). Currently it supports
[v0.5.0](https://github.com/toml-lang/toml/tree/v0.5.0)
of the TOML specification. It passes all of the validation tests in the
[validation suite](https://github.com/BurntSushi/toml-test), including various
0.5.0 examples.


## Installation

Use ``nimble`` to install it:

    nimble install parsetoml

## Usage

The full documentation is available at
http://parsetoml.readthedocs.org/en/latest/. Here I just show a few snippets of
code.

Import the library using

`````nim
import parsetoml
`````

There are several functions that can parse TOML content:

`````nim
let table1 = parsetoml.parseString(""""
[input]
file_name = "test.txt"

[output]
verbose = true
""")
let table2 = parsetoml.parseFile(f)
let table3 = parsetoml.parseFile("test.toml")
`````

The return value of ``parseString`` and ``parseFile`` is a ``TomlTableRef``
object. Several functions are available to query for specific fields; here is
an example:

`````nim
# Get the value, or fail if it is not found
let verboseFlag = parsetoml.getBool(table1, "output.verbose")

# You can specify a default as well
let input = parsetoml.getString(table1, "input.file_name", "dummy.txt")
`````

For the validation this library needs to output JSON, it therefore has a
procedure to produce JSON code. This is not just a simple JSON object however,
it follows the rather verbose specification of the validator.

`````nim
import parsetoml
import json
import streams

let table = parsetoml.parseStream(newFileStream(stdin))

echo table.toJson.pretty
`````

If you need to not only read TOML there is also the possibility of writing the
internal TOML representation out as a string. This is still an early
implementation and has not been run through the validator (it requires JSON
input). Therefore it probably contains bugs.

`````nim
import parsetoml
import streams

var table = parsetoml.parseString("""
[table]
a = 100
""")

var a = table.getValueFromFullAddr("table.a")
a.intVal = 200

echo table.toTomlString
`````
## License

Parsetoml is released under a MIT license.
