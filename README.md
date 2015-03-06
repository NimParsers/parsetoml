# NimToml

Parsetoml is a Nim library to parse TOML files
(https://github.com/toml-lang/toml). Currently it supports
[v0.3.1](https://github.com/toml-lang/toml/commit/bbada44e8c6d00e964cd6ca5b178507a34dcbe70)
of the TOML specification.

## Installation

Use ``nimble`` to install it:

    nimble install parsetoml

## Usage

The full documentation is available at
http://parsetoml.readthedocs.org/en/latest/. Here I just show a few snippets of
code.

Import the library using

`````nim
import NimToml
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

## License

Parsetoml is released under a MIT license.
