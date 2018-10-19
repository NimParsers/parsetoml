import macros
import unittest
import parsetoml

test "parseString":
  let table1 = parsetoml.parseString(""""
[input]
file_name = "test.txt"
""")
  check table1["input"]["file_name"] == "test.txt"
