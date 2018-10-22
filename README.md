# NimToml

Parsetoml is a Nim library to parse TOML files
(https://github.com/toml-lang/toml). Currently it supports
[v0.5.0](https://github.com/toml-lang/toml/tree/v0.5.0)
of the TOML specification. It passes all of the validation tests in the
[validation suite](https://github.com/BurntSushi/toml-test), including various
0.5.0 examples.


## Installation

Use `nimble` to install it:

    nimble install parsetoml

## Documentation

https://nimparsers.github.io/parsetoml/

## Usage

Below are just few snippets of code to show some basic usage of this
library. Refer to the above linked documentation for complete details.

### Importing the library

```nim
import parsetoml
```

### Parsing TOML content

```nim
let table1 = parsetoml.parseString("""
[input]
file_name = "test.txt"

[output]
verbose = true
""")
let table2 = parsetoml.parseFile(f)
let table3 = parsetoml.parseFile("test.toml")
```

### Using the parsed content

The return value of `parseString` and `parseFile` is a reference to
the `TomlValue` object, `TomlValueRef`.

Several *getter* procs are available to query for specific types of
fields for an input `TomlValueRef` variable:

- `getStr` : Get the string value.
- `getInt` : Get the integer value.
- `getFloat` : Get the float value.
- `getBool` : Get the bool value.
- `getElems` : Get a sequence of `TomlValueRef` values.
- `getTable` : Get a `TomlTableRef` value.

Using the same `table1` variable from the above example:

```nim
# Get the value, or fail if it is not found
let verboseFlag = table1["output"]["verbose"].getBool()

# You can specify a default as well
let input = table1["input"]["file_name"].getStr("some_default.txt")
```

### Transforming the parsed date to JSON / Table

For the validation this library needs to output JSON. Therefore it has
a proc to convert the `TomlValueRef` to JSON nodes.

```nim
import parsetoml, json

let table1 = parsetoml.parseString("""
[input]
file_name = "test.txt"

[output]
verbose = true
""")

echo table1.toJson.pretty()
```

Above outputs:

```json
{
  "input": {
    "file_name": {
      "type": "string",
      "value": "test.txt"
    }
  },
  "output": {
    "verbose": {
      "type": "bool",
      "value": "true"
    }
  }
}
```

To see the parsed TOML in an alternative Nim AST style indented
format, use `parsetoml.dump(table1.getTable())` with the above
example, and you will get:

```
input = table
    file_name = string("test.txt")
output = table
    verbose = boolean(true)
```

## License

Parsetoml is released under a MIT license.
