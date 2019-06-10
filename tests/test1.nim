import unittest
import sequtils
import json

# include parsetoml so that we can also test non exported procs
include parsetoml

suite "parse various TOML types":
  setup:
    let
      foo = parseString("""
some_integer = 123
some_float = 1.23
some_bool = true
some_string_array = ["abc", "def"]
some_int_array = [123, 456]
zero_int_array = [0, 1]
p_zero_array = [+0, 1]
m_zero_array = [-0, 1]
p_zero_float_array = [+0.0, 1.0]
m_zero_float_array = [-0.0, 1.0]

[input]
file_name = "test.txt"
""")

  test "TOML string":
    check:
      foo["input"]["file_name"].getStr() == "test.txt"

  test "TOML integer":
    check:
      foo["some_integer"].getInt() == 123

  test "TOML float":
    check:
      foo["some_float"].getFloat() == 1.23

  test "TOML bool":
    check:
      foo["some_bool"].getBool() == true

  test "TOML arrays":
    check:
      foo["some_string_array"].getElems().mapIt(it.getStr()) == @["abc", "def"]
      foo["some_int_array"].getElems().mapIt(it.getInt()) == @[123, 456]
      foo["zero_int_array"].getElems().mapIt(it.getInt()) == @[0, 1]
      foo["p_zero_array"].getElems().mapIt(it.getInt()) == @[0, 1]
      foo["m_zero_array"].getElems().mapIt(it.getInt()) == @[0, 1]
      foo["p_zero_float_array"].getElems().mapIt(it.getFloat()) == @[0'f64, 1]
      foo["m_zero_float_array"].getElems().mapIt(it.getFloat()) == @[0'f64, 1]

  test "TOML Table/JSON":
    let
      fooTable = foo.getTable()
      fooJson = fooTable.toJson()
    check:
      $fooJson["some_integer"] == """{"type":"integer","value":"123"}"""
      $fooJson["some_float"] == """{"type":"float","value":"1.23"}"""
      $fooJson["some_bool"] == """{"type":"bool","value":"true"}"""
      $fooJson["some_string_array"] == """{"type":"array","value":[{"type":"string","value":"abc"},{"type":"string","value":"def"}]}"""
      $fooJson["some_int_array"] == """{"type":"array","value":[{"type":"integer","value":"123"},{"type":"integer","value":"456"}]}"""
      $fooJson["input"] == """{"file_name":{"type":"string","value":"test.txt"}}"""

  test "TOML type constructors":
    check:
      newTString("Hello").kind == TomlValueKind.String
    check:
      newTInt(1234).kind == TomlValueKind.Int
    check:
      newTFloat(1.234).kind == TomlValueKind.Float
    check:
      newTBool(true).kind == TomlValueKind.Bool
    check:
      newTNull().kind == TomlValueKind.None
    check:
      newTTable().kind == TomlValueKind.Table
    check:
      newTArray().kind == TomlValueKind.Array

    # set array value
    var tomlRef: TomlValueRef
    const num = 3
    tomlRef.setArrayVal(num)
    check:
      tomlRef.kind == TomlValueKind.Array
      tomlRef.arrayVal.len == num
    # set empty table val
    tomlRef.setEmptyTableVal()
    check:
      tomlRef.kind == TomlValueKind.Table
