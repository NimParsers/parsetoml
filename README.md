# NimToml

NimToml is a Nim library to parse TOML files
(https://github.com/toml-lang/toml).

## Installation

Use ``nimble`` to install it:

    nimble install NimToml

## Usage

Import the library using

`````nim
import NimToml
`````

There are several functions that can parse TOML content:

`````nim
let table1 = NimToml.parseString(""""
[input]
file_name = "test.txt"

[output]
verbose = true
""")
let table2 = NimToml.parseFile(f)
let table3 = NimToml.parseFile("test.toml")
`````

The return value of ``parseString`` and ``parseFile`` is a
``NimToml.Table`` object, which can be either interrogated for
specific values or walked:

`````nim
# Get just one value:
let verboseFlag = NimToml.getBool(table1, "output.verbose")

# Iterate over all the values
for i in NimToml.iter(table1):
    print NimToml.getKey(i)

# The same, but run over all the table levels
# for i in NimToml.deepIter(table1):
    print NimToml.getKey(i)
`````

## License

NimToml is released under a MIT license.
