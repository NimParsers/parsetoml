# Copyright (c) 2015 Maurizio Tomasi
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# parsetoml - Nim library to parse a TOML file
# Main site: https://github.com/ziotom78/parsetoml

import math
import streams
import strutils
import tables
import unicode

type
  TomlValueKind* {.pure.} = enum
    None
    Int,
    Float,
    Bool,
    Datetime,
    String,
    Array,
    Table

  TomlDateTime* = object
    year*: int
    month*: int
    day*: int
    hour*: int
    minute*: int
    second*: int
    case shift: bool
    of true:
      isShiftPositive: bool
      zoneHourShift*: int
      zoneMinuteShift*: int
    of false: nil

  TomlTable* = OrderedTable[string, TomlValueRef]
  TomlTableRef* = ref TomlTable

  TomlValueRef* = ref TomlValue
  TomlValue* = object
    case kind*: TomlValueKind
    of TomlValueKind.None: nil
    of TomlValueKind.Int: intVal*: int64
    of TomlValueKind.Float: floatVal*: float64
    of TomlValueKind.Bool: boolVal*: bool
    of TomlValueKind.Datetime: dateTimeVal*: TomlDateTime
    of TomlValueKind.String: stringVal*: string
    of TomlValueKind.Array: arrayVal*: seq[TomlValueRef]
    of TomlValueKind.Table: tableVal*: TomlTableRef

  ParserState = object
    fileName*: string
    line*: int
    column*: int
    pushback: char
    stream*: streams.Stream

  TomlError* = object of Exception
    location*: ParserState

  NumberBase = enum
    base10, base16

  StringType {.pure.} = enum
    Basic,  # Enclosed within double quotation marks
    Literal # Enclosed within single quotation marks

const
  defaultStringCapacity = 256

proc newTomlError(location: ParserState, msg: string): ref TomlError =
  result = newException(TomlError, location.fileName & "(" & $location.line &
                        ":" & $location.column & ")" & " " & msg)
  result.location = location

proc getNextChar(state: var ParserState): char =
  # Return the next available char from the stream associate with
  # the parser state, or '\0' if there are no characters left.

  if state.pushback != '\0':
    # If we've just read a character without having interpreted
    # it, just return it
    result = state.pushback
    state.pushback = '\0'
  else:
    if state.stream.atEnd():
      return '\0'

    result = state.stream.readChar()

    # Update the line and column number
    if result == '\l':
      inc(state.line)
      state.column = 1
    elif result != '\r':
      inc(state.column)

proc pushBackChar(state: var ParserState, c: char) {.inline.} =
  state.pushback = c

type
  LfSkipMode = enum
    skipLf, skipNoLf

proc getNextNonWhitespace(state: var ParserState,
                          skip: LfSkipMode): char =
  # Note: this procedure does *not* consider a newline as a
  # "whitespace". Since newlines are often mandatory in TOML files
  # (e.g. after a key/value specification), we do not want to miss
  # them...

  let whitespaces = (case skip
                     of skipLf: {' ', '\t', '\r', '\l'}
                     of skipNoLf: {' ', '\t', '\r'})

  var nextChar: char
  while true:
    nextChar = state.getNextChar()
    if nextChar == '#':
      # Skip the comment up to the newline, but do not jump over it
      while nextChar != '\l':
        nextChar = state.getNextChar()

    if nextChar notin whitespaces: break

  result = nextChar

proc charToInt(c: char, base: NumberBase): int {.inline, noSideEffect.} =
  case base
  of base10: result = int(c) - int('0')
  of base16:
    if c in strutils.Digits:
      result = charToInt(c, base10)
    else:
      result = 10 + int(toUpperAscii(c)) - int('A')

type
  LeadingChar {.pure.} = enum
    AllowZero, DenyZero

proc parseInt(state: var ParserState,
              base: NumberBase,
              leadingChar: LeadingChar): int64 =
  var
    nextChar: char
    firstPos = true
    negative = false
    wasUnderscore = false

  let
    baseNum = (case base
               of base10: 10
               of base16: 16)
    digits = (case base
              of base10: strutils.Digits
              of base16: strutils.HexDigits)

  result = 0
  while true:
    wasUnderscore = nextChar == '_'
    nextChar = state.getNextChar()
    if nextChar == '_':
      if firstPos or wasUnderscore:
        raise(newTomlError(state,
                           "underscore must be surrounded by digit"))
      elif base == base16:
        raise(newTomlError(state,
                           "underscore not allowed in unicode"))
      continue

    if nextChar in {'+', '-'} and firstPos:
      firstPos = false
      if nextChar == '-': negative = true
      continue

    if nextChar == '0' and firstPos and leadingChar == LeadingChar.DenyZero:
      # TOML specifications forbid this
      raise(newTomlError(state,
                         "leading zeroes are not allowed in integers"))

    if nextChar notin digits:
      if wasUnderscore:
        raise(newTomlError(state,
                           "underscore must be surrounded by digit"))
      state.pushBackChar(nextChar)
      break

    try:
      result = result * baseNum - charToInt(nextChar, base)
    except OverflowError:
      raise(newTomlError(state,
                         "integer numbers wider than 64 bits not allowed"))

    firstPos = false

  if not negative:
    result = -result

proc parseDecimalPart(state: var ParserState): float64 =
  var
    nextChar: char
    invPowerOfTen = 10
    firstPos = true
    wasUnderscore = false

  result = 0.0
  while true:
    wasUnderscore = nextChar == '_'
    nextChar = state.getNextChar()
    if nextChar == '_':
      if firstPos or wasUnderscore:
        raise(newTomlError(state,
                           "underscore must be surrounded by digit"))
      continue
    if nextChar notin strutils.Digits:
      if wasUnderscore:
        raise(newTomlError(state,
                           "underscore must be surrounded by digit"))
      state.pushBackChar(nextChar)
      break

    result = result + (int(nextChar) - int('0')) / invPowerOfTen
    invPowerOfTen *= 10
    firstPos = false

proc stringDelimiter(kind: StringType): char {.inline, noSideEffect.} =
  result = (case kind
            of StringType.Basic: '\"'
            of StringType.Literal: '\'')

proc parseUnicode(state: var ParserState): string =
  let code = parseInt(state, base16, LeadingChar.AllowZero)
  if code notin 0'i64..0xD7FF and code notin 0xE000'i64..0x10FFFF:
    raise(newTomlError(state, "invalid Unicode codepoint, " &
                       "must be a Unicode scalar value"))

  return unicode.toUTF8(Rune(code))

proc parseEscapeChar(state: var ParserState, escape: char): string =
  case escape
  of 'b': result = "\b"
  of 't': result = "\t"
  of 'n': result = "\l"
  of 'f': result = "\f"
  of 'r': result = "\r"
  of '\'': result = "\'"
  of '\"': result = "\""
  of '\\': result = "\\"
  of 'u', 'U': result = parseUnicode(state)
  else:
    raise(newTomlError(state,
                       "unknown escape " &
                       "sequence \"\\" & escape & "\""))

proc parseSingleLineString(state: var ParserState, kind: StringType): string =
  # This procedure parses strings enclosed within single/double
  # quotation marks. It assumes that the quotation mark has already
  # been consumed by the "state" variable, which therefore is ready
  # to read the first character of the string.

  result = newStringOfCap(defaultStringCapacity)

  let delimiter = stringDelimiter(kind)

  var nextChar: char
  while true:
    nextChar = state.getNextChar()
    if nextChar == delimiter:
      break

    if nextChar == '\0':
      raise(newTomlError(state, "unterminated string"))

    if nextChar == '\\' and kind == StringType.Basic:
      nextChar = state.getNextChar()
      result.add(state.parseEscapeChar(nextChar))
      continue

    result.add(nextChar)

proc parseMultiLineString(state: var ParserState, kind: StringType): string =
  # This procedure parses strings enclosed within three consecutive
  # sigle/double quotation marks. It assumes that all the quotation
  # marks have already been consumed by the "state" variable, which
  # therefore is ready to read the first character of the string.

  result = newStringOfCap(defaultStringCapacity)
  let delimiter = stringDelimiter(kind)
  var
    isFirstChar = true
    nextChar: char
  while true:
    nextChar = state.getNextChar()

    # Skip the first newline, if it comes immediately after the
    # quotation marks
    if isFirstChar and (nextChar == '\l'):
      isFirstChar = false
      continue

    if nextChar == delimiter:
      # Are we done?
      nextChar = state.getNextChar()
      if nextChar == delimiter:
        nextChar = state.getNextChar()
        if nextChar == delimiter:
          # Done with this string
          return
        else:
          # Just got a double delimiter
          result.add(delimiter & delimiter)
          state.pushBackChar(nextChar)
          continue
      else:
        # Just got a lone delimiter
        result.add(delimiter)
        state.pushBackChar(nextChar)
        continue

    if nextChar == '\\' and kind == StringType.Basic:
      # This can either be an escape sequence or a end-of-line char
      nextChar = state.getNextChar()
      if nextChar in {'\l', '\r'}:
        # We're at the end of a line: skip everything till the
        # next non-whitespace character
        while nextChar in {'\l', '\r', ' ', '\t'}:
          nextChar = state.getNextChar()

        state.pushBackChar(nextChar)
        continue
      else:
        # This is just an escape sequence (like "\t")
        nextChar = state.getNextChar()
        result.add(state.parseEscapeChar(nextChar))
        continue

    result.add(nextChar)
    isFirstChar = false

proc parseString(state: var ParserState, kind: StringType): string =
  ## This function assumes that "state" has already consumed the
  ## first character (either \" or \', which is passed in the
  ## "openChar" parameter).

  let delimiter = stringDelimiter(kind)
  var nextChar: char = state.getNextChar()
  if nextChar == delimiter:
    # We have two possibilities here: (1) the empty string, or (2)
    # "long" multi-line strings.
    nextChar = state.getNextChar()
    if nextChar == delimiter:
      return parseMultiLineString(state, kind)
    else:
      # Empty string. This was easy!
      state.pushBackChar(nextChar)
      return ""
  else:
    state.pushBackChar(nextChar)
    return parseSingleLineString(state, kind)

# Forward declaration
proc parseValue(state: var ParserState): TomlValueRef

proc parseArray(state: var ParserState): seq[TomlValueRef] =
  # This procedure assumes that "state" has already consumed the '['
  # character

  result = newSeq[TomlValueRef](0)

  while true:
    var nextChar: char = state.getNextNonWhitespace(skipLf)
    case nextChar
    of ']':
      return
    of ',':
      if len(result) == 0:
        # This happens with "[, 1, 2]", for instance
        raise(newTomlError(state, "first array element missing"))

      # Check that this is not a terminating comma (like in
      #  "[b,]")
      nextChar = state.getNextNonWhitespace(skipLf)
      if nextChar == ']':
        return

      state.pushBackChar(nextChar)
    else:
      state.pushBackChar(nextChar)

      let
        oldState = state # Saved for error messages
        newValue = parseValue(state)

      if len(result) > 0:
        # Check that the type of newValue is compatible with the
        # previous ones
        if newValue.kind != result[low(result)].kind:
          raise(newTomlError(oldState,
                             "array members with incompatible types"))

      result.add(newValue)

proc parseIntAndCheckBounds(state: var ParserState,
                            minVal: int,
                            maxVal: int,
                            msg: string): int =
  result = int(parseInt(state, base10, LeadingChar.AllowZero))
  if result < minVal or result > maxVal:
    raise(newTomlError(state, msg & " (" & $result & ")"))

proc parseDateTimePart(state: var ParserState,
                       dateTime: var TomlDateTime) =

  # This function is called whenever a datetime object is found. They follow
  # an ISO convention and can use one of the following format:
  #
  # - YYYY-MM-DDThh:mm:ss[+-]hh:mm
  # - YYYY-MM-DDThh:mm:ssZ
  #
  # where the "T" and "Z" letters are literals, [+-] indicates
  # *either* "+" or "-", YYYY is the 4-digit year, MM is the 2-digit
  # month, DD is the 2-digit day, hh is the 2-digit hour, mm is the
  # 2-digit minute, and ss is the 2-digit second. The hh:mm after
  # the +/- character is the timezone; a literal "Z" indicates the
  # local timezone.

  # This function assumes that the "YYYY-" part has already been
  # parsed (this happens because during parsing, finding a 4-digit
  # number like "YYYY" might just indicate the presence of an
  # integer or a floating-point number; it's the following "-" that
  # tells the parser that the value is a datetime). As a consequence
  # of this, we assume that "dateTime.year" has already been set.

  var
    nextChar: char
    lastChar: int

  # Parse the month
  lastChar = state.column
  dateTime.month = parseIntAndCheckBounds(state, 1, 12,
                                          "invalid number for the month")
  if state.column - lastChar != 3:
    echo $(state.column - lastChar)
    raise(newTomlError(state, "month number is not exactly two digits"))

  nextChar = state.getNextChar()
  if nextChar != '-':
    raise(newTomlError(state, "\"-\" expected after the month number"))

  # Parse the day
  lastChar = state.column
  dateTime.day = parseIntAndCheckBounds(state, 1, 31,
                                        "invalid number for the day")
  if state.column - lastChar != 3:
    raise(newTomlError(state, "day number is not exactly two digits"))

  nextChar = state.getNextChar()
  if nextChar notin {'t', 'T'}:
    raise(newTomlError(state, "\"T\" expected after the day number"))

  # Parse the hour
  lastChar = state.column
  dateTime.hour = parseIntAndCheckBounds(state, 0, 23,
                                         "invalid number of hours")
  if state.column - lastChar != 3:
    raise(newTomlError(state, "hours is not exactly two digits"))

  nextChar = state.getNextChar()
  if nextChar != ':':
    raise(newTomlError(state, "\":\" expected after the number of hours"))

  # Parse the minutes
  lastChar = state.column
  dateTime.minute = parseIntAndCheckBounds(state, 0, 59,
                                           "invalid number of minutes")
  if state.column - lastChar != 3:
    raise(newTomlError(state, "minutes is not exactly two digits"))

  nextChar = state.getNextChar()
  if nextChar != ':':
    raise(newTomlError(state,
                       "\":\" expected after the number of seconds"))

  # Parse the second. Note that seconds=60 *can* happen (leap second)
  lastChar = state.column
  dateTime.second = parseIntAndCheckBounds(state, 0, 60, "invalid second")
  if state.column > lastChar and
     state.column - lastChar != 3:
    raise(newTomlError(state, "seconds is not exactly two digits"))

  nextChar = state.getNextChar()
  case nextChar
  of 'z', 'Z':
    dateTime.shift = false
  of '+', '-':
    dateTime.shift = true
    dateTime.isShiftPositive = (nextChar == '+')
    lastChar = state.column
    dateTime.zoneHourShift =
      parseIntAndCheckBounds(state, 0, 23,
                             "invalid number of shift hours")
    if state.column - lastChar != 3:
      raise(newTomlError(state, "shift hours is not exactly two digits"))

    nextChar = state.getNextChar()
    if nextChar != ':':
      raise(newTomlError(state,
                         "\":\" expected after the number of shift hours"))

    lastChar = state.column
    dateTime.zoneMinuteShift =
      parseIntAndCheckBounds(state, 0, 59,
                             "invalid number of shift minutes")
  else:
    raise(newTomlError(state, "unexpected character \"" & nextChar &
                       "\" instead of the time zone"))

proc pow10(x: float64, pow: int64): float64 {.inline.} =
  if pow == 0:
    result = x
    return

  let mulFactor = if pow < 0:
                    0.1'f64
                  else:
                    10.0'f64

  result = x
  for idx in countup(1'i64, abs(pow)):
    result *= mulFactor

proc parseValue(state: var ParserState): TomlValueRef =
  var nextChar: char

  nextChar = state.getNextNonWhitespace(skipNoLf)
  case nextChar
  of strutils.Digits, '+', '-':
    let surelyNotDateTime = nextChar in {'+', '-'}
    state.pushBackChar(nextChar)

    # We can either have an integer, a float or a datetime
    let intPart = parseInt(state, base10, LeadingChar.DenyZero)
    nextChar = state.getNextChar()
    case nextChar
    of 'e', 'E':
      let exponent = parseInt(state, base10, LeadingChar.AllowZero)
      let value = pow10(float64(intPart), exponent)
      result = TomlValueRef(kind: TomlValueKind.Float,
                            floatVal: value)
    of '.':
      nextChar = state.getNextChar()
      if nextChar notin strutils.Digits:
        raise(newTomlError(state, "dot not followed by digit in float"))
      state.pushBackChar(nextChar)
      let decimalPart = parseDecimalPart(state)
      nextChar = state.getNextChar()
      var exponent: int64 = 0
      if nextChar in {'e', 'E'}:
        exponent = parseInt(state, base10, LeadingChar.AllowZero)
      else:
        state.pushBackChar(nextChar)

      let value =
        if intPart < 0:
          pow10(float64(intPart) - decimalPart, exponent)
        else:
          pow10(float64(intPart) + decimalPart, exponent)
      result = TomlValueRef(kind: TomlValueKind.Float,
                            floatVal: value)
    of '-':
      if surelyNotDateTime:
        raise(newTomlError(state, "unexpected character \"-\""))

      # This might be a datetime object
      var val: TomlDateTime
      val.year = int(intPart)
      # We assume a year has 4 digits
      if val.year < 1000 or val.year > 9999:
        raise(newTomlError(state, "invalid year (" & $val.year & ")"))

      parseDateTimePart(state, val)

      result = TomlValueRef(kind: TomlValueKind.DateTime,
                            dateTimeVal: val)
    else:
      state.pushBackChar(nextChar)
      result = TomlValueRef(kind: TomlValueKind.Int,
                            intVal: intPart)

  of 't':
    # Is this "true"?
    let oldState = state # Only used for error messages
    if state.getNextChar() != 'r' or
       state.getNextChar() != 'u' or
       state.getNextChar() != 'e':
        raise(newTomlError(oldState, "unknown identifier"))
    result = TomlValueRef(kind: TomlValueKind.Bool, boolVal: true)

  of 'f':
    # Is this "false"?
    let oldState = state # Only used for error messages
    if state.getNextChar() != 'a' or
       state.getNextChar() != 'l' or
       state.getNextChar() != 's' or
       state.getNextChar() != 'e':
        raise(newTomlError(oldState, "unknown identifier"))
    result = TomlValueRef(kind: TomlValueKind.Bool, boolVal: false)

  of '\"':
    # A basic string (accepts \ escape codes)
    result = TomlValueRef(kind: TomlValueKind.String,
                          stringVal: parseString(state, StringType.Basic))

  of '\'':
    # A literal string (does not accept \ escape codes)
    result = TomlValueRef(kind: TomlValueKind.String,
                          stringVal: parseString(state, StringType.Literal))

  of '[':
    # An array
    result = TomlValueRef(kind: TomlValueKind.Array,
                          arrayVal: parseArray(state))

  else:
    raise(newTomlError(state,
                       "unexpected character \"" & nextChar & "\""))

proc parseName(state: var ParserState): string =
  # This parses the name of a key or a table
  result = newStringOfCap(defaultStringCapacity)

  var nextChar = state.getNextChar()
  if nextChar == '\"':
    return state.parseString(StringType.Basic)
  elif nextChar == '\'':
    return state.parseString(StringType.Literal)
  state.pushBackChar(nextChar)
  while true:
    nextChar = state.getNextChar()
    if (nextChar in {'=', '.', '[', ']', '\0', ' '}):
      # Any of the above characters marks the end of the name
      state.pushBackChar(nextChar)
      break
    elif (nextChar notin {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}):
      raise(newTomlError(state,
                         "bare key has illegal character"))
    else:
      result.add(nextChar)

type
  BracketType {.pure.} = enum
    single, double

proc parseTableName(state: var ParserState,
                    brackets: BracketType): seq[string] =
  # This code assumes that '[' has already been consumed
  result = newSeq[string](0)

  while true:
    #let partName = state.parseName(SpecialChars.AllowNumberSign)
    var
      nextChar = state.getNextChar()
      partName: string
    if nextChar == '"':
      partName = state.parseString(StringType.Basic)
    else:
      state.pushBackChar(nextChar)
      partName = state.parseName()
    result.add(partName)

    nextChar = state.getNextChar()
    case nextChar
    of ']':
      if brackets == BracketType.double:
        nextChar = state.getNextChar()
        if nextChar != ']':
          raise(newTomlError(state,
                             "\"]]\" expected"))

      # We must check that there is nothing else in this line
      nextChar = state.getNextNonWhitespace(skipNoLf)
      if nextChar != '\l':
        raise(newTomlError(state,
                           "unexpected character \"" & nextChar & "\""))

      break

    of '.': continue
    else:
      raise(newTomlError(state,
                         "unexpected character \"" & nextChar & "\""))

proc parseInlineTable(state: var ParserState, tableRef: var TomlTableRef) =
  while true:
    var nextChar = state.getNextNonWhitespace(skipLf)
    case nextChar
    of '}':
      return
    of ',':
      # Check that this is not a terminating comma (like in
      #  "[b,]")
      nextChar = state.getNextNonWhitespace(skipLf)
      if nextChar == '}':
        return

      state.pushBackChar(nextChar)
    else:
      state.pushBackChar(nextChar)

      let key = state.parseName()

      nextChar = state.getNextNonWhitespace(skipLf)
      if nextChar != '=':
        raise(newTomlError(state,
                           "key names cannot contain spaces"))
      let value = state.parseValue()
      (tableRef[])[key] = value

proc createTableDef(state: ParserState,
                    curTableRef: var TomlTableRef,
                    tableNames: seq[string])

proc parseKeyValuePair(state: var ParserState, tableRef: var TomlTableRef) =
  let key = state.parseName()

  var nextChar = state.getNextNonWhitespace(skipNoLf)
  if nextChar != '=':
    raise(newTomlError(state,
                       "key names cannot contain spaces"))

  nextChar = state.getNextNonWhitespace(skipNoLf)
  # Check that this is a regular value and not an inline table
  if nextChar != '{':
    state.pushBackChar(nextChar)
    let value = state.parseValue()

    # We must check that there is nothing else in this line
    nextChar = state.getNextNonWhitespace(skipNoLf)
    if nextChar != '\l':
      raise(newTomlError(state,
                         "unexpected character \"" & nextChar & "\""))

    if (tableRef[]).hasKey(key):
      raise(newTomlError(state,
                         "duplicate key, \"" & key & "\" already in table"))
    (tableRef[])[key] = value
  else:
    nextChar = state.getNextNonWhitespace(skipNoLf)
    if nextChar == ',':
      raise(newTomlError(state, "first input table element missing"))
    state.pushBackChar(nextChar)
    let oldTableRef = tableRef
    createTableDef(state, tableRef, @[key])
    parseInlineTable(state, tableRef)
    tableRef = oldTableRef

proc newParserState(s: streams.Stream,
                    fileName: string = ""): ParserState =
  result = ParserState(fileName: fileName, line: 1, column: 1, stream: s)

proc setEmptyTableVal(val: TomlValueRef) =
  val.kind = TomlValueKind.Table
  new(val.tableVal)
  val.tableVal[] = initOrderedTable[string, TomlValueRef]()

proc setArrayVal(val: TomlValueRef, numOfElems: int = 0) =
  val.kind = TomlValueKind.Array
  val.arrayVal = newSeq[TomlValueRef](numOfElems)

proc advanceToNextNestLevel(state: ParserState,
                            curTableRef: var TomlTableRef,
                            tableName: string) =
  let target = (curTableRef[])[tableName]
  case target.kind
  of TomlValueKind.Table:
    curTableRef = target.tableVal
  of TomlValueKind.Array:
    let arr = target.arrayVal[high(target.arrayVal)]
    if arr.kind != TomlValueKind.Table:
      raise(newTomlError(state, "\"" & tableName &
                         "\" elements are not tables"))
    curTableRef = arr.tableVal
  else:
    raise(newTomlError(state, "\"" & tableName &
                       "\" is not a table"))

# This function is called by the TOML parser whenever a
# "[[table.name]]" line is encountered in the parsing process. Its
# purpose is to make sure that all the parent nodes in "table.name"
# exist and are tables, and that a terminal node of the correct type
# is created.
#
# Starting from "curTableRef" (which is usually the root object),
# traverse the object tree following the names in "tableNames" and
# create a new TomlValueRef object of kind "TomlValueKind.Array" at
# the terminal node. This array is going to be an array of tables: the
# function will create an element and will make "curTableRef"
# reference it. Example: if tableNames == ["a", "b", "c"], the code
# will look for the "b" table that is child of "a", and then it will
# check if "c" is a child of "b". If it is, it must be an array of
# tables, and a new element will be appended. Otherwise, a new "c"
# array is created, and an empty table element is added in "c". In
# either cases, curTableRef will refer to the last element of "c".

proc createOrAppendTableArrayDef(state: ParserState,
                                 curTableRef: var TomlTableRef,
                                 tableNames: seq[string]) =
  # This is a table array entry (e.g. "[[entry]]")
  for idx, tableName in tableNames:
    if tableName.len == 0:
      raise(newTomlError(state,
                         "empty key not allowed"))
    let lastTableInChain = idx == high(tableNames)

    var newValue: TomlValueRef
    if not hasKey(curTableRef[], tableName):
      # If this element does not exist, create it
      new(newValue)

      # If this is the last name in the chain (e.g.,
      # "c" in "a.b.c"), its value should be an
      # array of tables, otherwise just a table
      if lastTableInChain:
        setArrayVal(newValue, 1)

        new(newValue.arrayVal[0])
        setEmptyTableVal(newValue.arrayVal[0])

        (curTableRef[])[tableName] = newValue
        curTableRef = newValue.arrayVal[0].tableVal
      else:
        setEmptyTableVal(newValue)

        # Add the newly created object to the current table
        (curTableRef[])[tableName] = newValue

        # Update the pointer to the current table
        curTableRef = newValue.tableVal
    else:
      # The element exissts: is it of the right type?
      let target = (curTableRef[])[tableName]

      if lastTableInChain:
        if target.kind != TomlValueKind.Array:
          raise(newTomlError(state, "\"" & tableName &
                                    " is not an array"))

        var newValue: TomlValueRef
        new(newValue)
        setEmptyTableVal(newValue)
        target.arrayVal.add(newValue)
        curTableRef = newValue.tableVal
      else:
        advanceToNextNestLevel(state, curTableRef, tableName)

# Starting from "curTableRef" (which is usually the root object),
# traverse the object tree following the names in "tableNames" and
# create a new TomlValueRef object of kind "TomlValueKind.Table" at
# the terminal node. Example: if tableNames == ["a", "b", "c"], the
# code will look for the "b" table that is child of "a" and it will
# create a new table "c" which is "b"'s children.

proc createTableDef(state: ParserState,
                    curTableRef: var TomlTableRef,
                    tableNames: seq[string]) =
  var newValue: TomlValueRef

  # This starts a new table (e.g. "[table]")
  for tableName in tableNames:
    if tableName.len == 0:
      raise(newTomlError(state,
                         "empty key not allowed"))
    if not hasKey(curTableRef[], tableName):
      new(newValue)
      setEmptyTableVal(newValue)

      # Add the newly created object to the current table
      (curTableRef[])[tableName] = newValue

      # Update the pointer to the current table
      curTableRef = newValue.tableVal
    else:
      advanceToNextNestLevel(state, curTableRef, tableName)

proc parseStream*(inputStream: streams.Stream,
                  fileName: string = ""): TomlTableRef =
  ## Parses a stream of TOML formatted data into a TOML table. The optional
  ## filename is used for error messages.
  var state = newParserState(inputStream, fileName)
  new(result)
  result[] = initOrderedTable[string, TomlValueRef]()

  # This pointer will always point to the table that should get new
  # key/value pairs found in the TOML file during parsing
  var curTableRef = result

  # Unlike "curTableRef", this pointer never changes: it always
  # points to the uppermost table in the tree
  let baseTable = result

  var nextChar: char
  while true:
    nextChar = state.getNextNonWhitespace(skipLf)
    case nextChar
    of '[':
      # A new section/table begins. We'll have to start again
      # from the uppermost level, so let's rewind curTableRef to
      # the root node
      curTableRef = baseTable

      # First, decompose the table name into its part (e.g.,
      # "a.b.c" -> ["a", "b", "c"])
      nextChar = state.getNextChar()
      let isTableArrayDef = nextChar == '['
      var tableNames: seq[string]
      if isTableArrayDef:
        tableNames = state.parseTableName(BracketType.double)
      else:
        state.pushBackChar(nextChar)
        tableNames = state.parseTableName(BracketType.single)

      # Now create the proper (empty) data structure: either a
      # table or an array of tables. Note that both functions
      # update the "curTableRef" variable: they have to, since
      # the TOML specification says that any "key = value"
      # statement that follows is a child of the table we're
      # defining right now, and we use "curTableRef" as a
      # reference to the table that gets every next key/value
      # definition.
      if isTableArrayDef:
        createOrAppendTableArrayDef(state, curTableRef, tableNames)
      else:
        createTableDef(state, curTableRef, tableNames)

    of '=':
      raise(newTomlError(state, "key name missing"))
    of '#', '.', ']':
      raise(newTomlError(state,
                         "unexpected character \"" & nextChar & "\""))
    of '\0': # EOF
      return
    else:
      # Everything else marks the presence of a "key = value" pattern
      state.pushBackChar(nextChar)
      parseKeyValuePair(state, curTableRef)

proc parseString*(tomlStr: string, fileName: string = ""): TomlTableRef =
  ## Parses a string of TOML formatted data into a TOML table. The optional
  ## filename is used for error messages.
  let strStream = newStringStream(tomlStr)
  result = parseStream(strStream, fileName)

proc parseFile*(f: File, fileName: string = ""): TomlTableRef =
  ## Parses a file of TOML formatted data into a TOML table. The optional
  ## filename is used for error messages.
  let fStream = newFileStream(f)
  result = parseStream(fStream, fileName)

proc parseFile*(fileName: string): TomlTableRef =
  ## Parses the file found at fileName with TOML formatted data into a TOML
  ## table.
  let fStream = newFileStream(fileName, fmRead)
  result = parseStream(fStream, fileName)

proc `$`*(val: TomlDateTime): string =
  ## Converts the TOML date-time object into the ISO format read by the parser
  result = ($val.year).align(4, '0') & "-" & ($val.month).align(2, '0') & "-" &
    ($val.day).align(2, '0') & "T" & ($val.hour).align(2, '0') & ":" &
    ($val.minute).align(2, '0') & ":" & ($val.second).align(2, '0') &
    (if not val.shift: "Z" else: (
      (if val.isShiftPositive: "+" else: "-") &
        ($val.zonehourshift).align(2, '0') & ":" &
        ($val.zoneminuteshift).align(2, '0'))
    )

proc `$`*(val: TomlValue): string =
  ## Turns whatever value into a type and value representation, used by ``dump``
  case val.kind
  of TomlValueKind.None:
    result = "none()"
  of TomlValueKind.Int:
    result = "int(" & $val.intVal & ")"
  of TomlValueKind.Float:
    result = "float(" & $val.floatVal & ")"
  of TomlValueKind.Bool:
    result = "boolean(" & $val.boolVal & ")"
  of TomlValueKind.Datetime:
    result = "datetime(" & $val.datetimeVal & ")"
  of TomlValueKind.String:
    result = "string(\"" & $val.stringVal & "\")"
  of TomlValueKind.Array:
    result = "array("
    for elem in val.arrayVal:
      result.add($(elem[]))
    result.add(")")
  of TomlValueKind.Table:
    result = "table(" & $(len(val.tableVal)) & " elements)"

proc dump*(table: TomlTableRef, indentLevel: int = 0) =
  ## Dump out the entire table as it was parsed. This procedure is mostly
  ## useful for debugging purposes
  let space = spaces(indentLevel)
  for key, val in pairs(table):
    if val.kind == TomlValueKind.Table:
      echo space & key & " = table"
      dump(val.tableVal, indentLevel + 4)
    elif (val.kind == TomlValueKind.Array and
        val.arrayVal[0].kind == TomlValueKind.Table):
      for idx, val in val.arrayVal:
        echo space & key & "[" & $idx & "] = table"
        dump(val.tableVal, indentLevel + 4)
    else:
      echo space & key & " = " & $(val[])

proc newNoneValue(): TomlValueRef =
  new(result)
  result.kind = TomlValueKind.None

proc getValueFromFullAddr*(table: TomlTableRef,
                           fullAddr: string): TomlValueRef =
  ## Given a TOML table reference and a string address, return a
  ## reference to the value in the table. Addresses are of the form
  ## "a.b.c.d", where all but the last element in the dot-separated
  ## string are tables. Elements in table arrays are indicated by the
  ## form "a[NNN]", where NNN is an integer number.

  let fieldNames = strutils.split(fullAddr, '.')

  var
    curTable: ref TomlTable = table
    curNode: TomlValueRef
  for idx, curFieldName in fieldNames:
    let isLast = idx == high(fieldNames)
    # Check if this name is a reference to an array element (e.g., "a[2]")
    let indexPos = curFieldName.find("[")
    if indexPos > 0 and curFieldName.endsWith("]"):
      # This is the name of the array
      let arrayName = curFieldName[0..indexPos-1]
      # This is the index within the square brackets
      let indexStr = curFieldName[indexPos+1..len(curFieldName)-2]
      try:
        # Within this "try..except" statement we do not check
        # for errors
        let index = strutils.parseInt(indexStr)
        curNode = curTable[arrayName].arrayVal[index]
      except:
        return newNoneValue()
    else:
      if not curTable.hasKey(curFieldName):
        return newNoneValue()

      curNode = curTable[curFieldName]

    if not isLast:
      case curNode.kind
      of TomlValueKind.Table:
        curTable = curNode.tableVal
      else:
        return newNoneValue()

  result = curNode

template defineGetProc(name: untyped,
                       kindVal: TomlValueKind,
                       field: untyped,
                       t: typeDesc,
                       doccomment: untyped) =
  proc name*(table: TomlTableRef,
             address: string): t =
    doccomment
    let node = table.getValueFromFullAddr(address)
    if node.kind == TomlValueKind.None:
      raise(newException(KeyError, "key \"" & address & "\" not found"))

    if node.kind == kindVal:
      result = node.field
    else:
      raise(newException(ValueError, "key \"" & address &
                                   "\" has the wrong type"))

template defineGetProcDefault(name: untyped,
                              t: typeDesc,
                              doccomment: untyped) =
  proc name*(table: TomlTableRef,
             address: string,
             default: t): t =
    doccomment
    try:
      result = name(table, address)
    except KeyError:
      result = default

defineGetProc(getInt, TomlValueKind.Int, intVal, int64) do:
  ## Get an integer from the table indicated by the address string. This works
  ## like ``getValueFromFullAddr`` but does extra validation. If there is no
  ## value found at the address a KeyError is thrown, if it is found, but it has
  ## the wrong type, a ValueError is thrown.
defineGetProc(getFloat, TomlValueKind.Float, floatVal, float64) do:
  ## Get a float from the table indicated by the address string. This works
  ## like ``getValueFromFullAddr`` but does extra validation. If there is no
  ## value found at the address a KeyError is thrown, if it is found, but it has
  ## the wrong type, a ValueError is thrown.
defineGetProc(getBool, TomlValueKind.Bool, boolVal, bool) do:
  ## Get a boolean from the table indicated by the address string. This works
  ## like ``getValueFromFullAddr`` but does extra validation. If there is no
  ## value found at the address a KeyError is thrown, if it is found, but it has
  ## the wrong type, a ValueError is thrown.
defineGetProc(getString, TomlValueKind.String, stringVal, string) do:
  ## Get a string from the table indicated by the address string. This works
  ## like ``getValueFromFullAddr`` but does extra validation. If there is no
  ## value found at the address a KeyError is thrown, if it is found, but it has
  ## the wrong type, a ValueError is thrown.
defineGetProc(getDateTime, TomlValueKind.DateTime, dateTimeVal,
  TomlDateTime) do:
  ## Get a TomlDateTime object from the table indicated by the address string.
  ## This works like ``getValueFromFullAddr`` but does extra validation. If
  ## there is no value found at the address a KeyError is thrown, if it is
  ## found, but it has the wrong type, a ValueError is thrown.
defineGetProc(getTable, TomlValueKind.Table, tableVal, TomlTableRef) do:
  ## Get a sub-table from the table indicated by the address string. This works
  ## like ``getValueFromFullAddr`` but does extra validation. If there is no
  ## value found at the address a KeyError is thrown, if it is found, but it has
  ## the wrong type, a ValueError is thrown.



defineGetProcDefault(getInt, int64) do:
  ## Similar to `getInt` but will return the default value if no value with
  ## that name was found. This will still throw ValueErrors if the value found
  ## at the address has the wrong type.
defineGetProcDefault(getFloat, float64) do:
  ## Similar to `getFloat` but will return the default value if no value with
  ## that name was found. This will still throw ValueErrors if the value found
  ## at the address has the wrong type.
defineGetProcDefault(getBool, bool) do:
  ## Similar to `getBool` but will return the default value if no value with
  ## that name was found. This will still throw ValueErrors if the value found
  ## at the address has the wrong type.
defineGetProcDefault(getString, string) do:
  ## Similar to `getString` but will return the default value if no value with
  ## that name was found. This will still throw ValueErrors if the value found
  ## at the address has the wrong type.

template defineGetArray(name: untyped,
                        kindVal: TomlValueKind,
                        field: untyped,
                        t: typeDesc,
                        doccomment: untyped) =
  proc name*(table: TomlTableRef,
             address: string): seq[t] =
    doccomment
    let node = table.getValueFromFullAddr(address)
    case node.kind
    of TomlValueKind.None:
      raise(newException(KeyError, "key \"" & address & "\" not found"))
    of TomlValueKind.Array:
      let arr = node.arrayVal
      if arr.len() == 0:
        result = newSeq[t](0)
        return

      if arr[0].kind != kindVal:
        raise(newException(ValueError, "the array elements of \"" &
                                     address &
                                     "\" have the wrong type (" &
                                     $kindVal & ")"))

      result = newSeq[t](len(arr))
      for idx, elem in arr:
        result[idx] = elem.field
    else:
      raise(newException(ValueError, "key \"" & address &
                                   "\" is not an array"))

defineGetArray(getIntArray, TomlValueKind.Int, intVal, int64) do:
  ## Get an array of integer values from the table indicated by the address
  ## string. This works like ``getValueFromFullAddr`` but does extra validation.
  ## If there is no value found at the address a KeyError is thrown, if it is
  ## found, but it is not an array or the values in the array have the wrong
  ## type, a ValueError is thrown.
defineGetArray(getFloatArray, TomlValueKind.Float, floatVal, float64) do:
  ## Get an array of float values from the table indicated by the address
  ## string. This works like ``getValueFromFullAddr`` but does extra validation.
  ## If there is no value found at the address a KeyError is thrown, if it is
  ## found, but it is not an array or the values in the array have the wrong
  ## type, a ValueError is thrown.
defineGetArray(getBoolArray, TomlValueKind.Bool, boolVal, bool) do:
  ## Get an array of boolean values from the table indicated by the address
  ## string. This works like ``getValueFromFullAddr`` but does extra validation.
  ## If there is no value found at the address a KeyError is thrown, if it is
  ## found, but it is not an array or the values in the array have the wrong
  ## type, a ValueError is thrown.
defineGetArray(getStringArray, TomlValueKind.String, stringVal, string) do:
  ## Get an array of string values from the table indicated by the address
  ## string. This works like ``getValueFromFullAddr`` but does extra validation.
  ## If there is no value found at the address a KeyError is thrown, if it is
  ## found, but it is not an array or the values in the array have the wrong
  ## type, a ValueError is thrown.
defineGetArray(getDateTimeArray, TomlValueKind.DateTime,
               dateTimeVal, TomlDateTime) do:
  ## Get an array of DateTime objects from the table indicated by the address
  ## string. This works like ``getValueFromFullAddr`` but does extra validation.
  ## If there is no value found at the address a KeyError is thrown, if it is
  ## found, but it is not an array or the values in the array have the wrong
  ## type, a ValueError is thrown.

import json, sequtils

proc toJson*(value: TomlValueRef): JsonNode

proc toJson*(table: TomlTableRef): JsonNode =
  ## Converts a TOML table to a JSON node. This uses the format specified in
  ## the validation suite for it's output:
  ## https://github.com/BurntSushi/toml-test#example-json-encoding
  result = newJObject()
  for key, value in pairs(table):
    result[key] = value.toJson

proc toJson*(value: TomlValueRef): JsonNode =
  ## Converts a TOML value to a JSON node. This uses the format specified in
  ## the validation suite for it's output:
  ## https://github.com/BurntSushi/toml-test#example-json-encoding
  case value.kind:
    of TomlValueKind.Int:
      %*{"type": "integer", "value": $value.intVal}
    of TomlValueKind.Float:
      %*{"type": "float", "value": $value.floatVal}
    of TomlValueKind.Bool:
      %*{"type": "bool", "value": $value.boolVal}
    of TomlValueKind.Datetime:
      %*{"type": "datetime", "value": $value.datetimeVal}
    of TomlValueKind.String:
      %*{"type": "string", "value": value.stringVal}
    of TomlValueKind.Array:
      if value.arrayVal.len == 0:
        %*{"type": "array", "value": []}
      elif value.arrayVal[0].kind == TomlValueKind.Table:
        %value.arrayVal.map(toJson)
      else:
        %*{"type": "array", "value": value.arrayVal.map(toJson)}
    of TomlValueKind.Table:
      value.tableVal.toJson
    of TomlValueKind.None:
      %*{"type": "ERROR"}

proc toKey(str: string): string =
  for c in str:
    if (c notin {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}):
      return "\"" & str & "\""
  str

proc toTomlString*(value: TomlValueRef): string

proc toTomlString*(value: TomlTableRef, parents = ""): string =
  ## Converts a TOML table to a TOML formatted string for output to a file.
  result = ""
  var subtables: seq[tuple[key: string, value: TomlValueRef]] = @[]
  for key, value in pairs(value):
    block outer:
      if value.kind == TomlValueKind.Table:
        subtables.add((key: key, value: value))
      elif value.kind == TomlValueKind.Array and
        value.arrayVal[0].kind == TomlValueKind.Table:
        let tables = value.arrayVal.map(toTomlString)
        for table in tables:
          result = result & "[[" & key & "]]\n" & table & "\n"
      else:
        result = result & key.toKey & " = " & toTomlString(value) & "\n"
  for kv in subtables:
    let fullKey = (if parents.len > 0: parents & "." else: "") & kv.key.toKey
    for ikey, ivalue in pairs(kv.value.tableVal):
      if ivalue.kind != TomlValueKind.Table:
        return result & "[" & fullKey & "]\n" &
          kv.value.tableVal.toTomlString(fullKey) & "\n"
    result = result & kv.value.tableVal.toTomlString(fullKey)

proc toTomlString*(value: TomlValueRef): string =
  ## Converts a TOML value to a TOML formatted string for output to a file.
  case value.kind:
  of TomlValueKind.Int: $value.intVal
  of TomlValueKind.Float: $value.floatVal
  of TomlValueKind.Bool: $value.boolVal
  of TomlValueKind.Datetime: $value.datetimeVal
  of TomlValueKind.String: "\"" & value.stringVal & "\""
  of TomlValueKind.Array:
    if value.arrayVal.len == 0:
      "[]"
    elif value.arrayVal[0].kind == TomlValueKind.Table:
      value.arrayVal.map(toTomlString).join("\n")
    else:
      "[" & value.arrayVal.map(toTomlString).join(", ") & "]"
  of TomlValueKind.Table: value.tableVal.toTomlString
  else:
    "UNKNOWN"

when isMainModule:
  template assertEq(T1: untyped, T2: untyped) =
    bind instantiationInfo
    mixin failedAssertImpl
    when compileOption("assertions"):
      {.line.}:
        let val1 = T1
        let val2 = T2
        if not (val1 == val2):
          failedAssertImpl(astToStr(T1) & " != " & astToStr(T2) &
                           " (" &
                           $(val1) & " != " & $(val2) & ')')

  # Here come a few tests

  # pow10
  assert pow10(5.0, 1) == 50.0
  assert pow10(5.0, 2) == 500.0
  assert pow10(5.0, 3) == 5000.0

  assert pow10(100.0, -1) == 10.0
  assert pow10(100.0, -2) == 1.0

  # getNextChar

  block:
    var s = newParserState(newStringStream("""
ab c
de"""))
    assert(s.line == 1 and s.column == 1)

    assertEq(s.getNextChar(), 'a')
    assert(s.line == 1 and s.column == 2)

    assertEq(s.getNextChar(), 'b')
    assert(s.line == 1 and s.column == 3)

    assertEq(s.getNextChar(), ' ')
    assert(s.line == 1 and s.column == 4)

    assertEq(s.getNextChar(), 'c')
    assert(s.line == 1 and s.column == 5)

    # Let's add some juice to this boring test...
    s.pushBackChar('d')
    assertEq(s.getNextChar(), 'd')
    assert(s.line == 1 and s.column == 5)

    assertEq(s.getNextChar(), '\l')
    assert(s.line == 2 and s.column == 1)

    assertEq(s.getNextChar(), 'd')
    assert(s.line == 2 and s.column == 2)

    assertEq(s.getNextChar(), 'e')
    assert(s.line == 2 and s.column == 3)

    assertEq(s.getNextChar(), '\0')

  # getNextNonWhitespace

  block:
    var s = newParserState(newStringStream("ab c\td # Comment\ne\rf"))

    assert(s.line == 1 and s.column == 1)

    assertEq(s.getNextNonWhitespace(skipNoLf), 'a')
    assert(s.line == 1 and s.column == 2)

    assertEq(s.getNextNonWhitespace(skipNoLf), 'b')
    assert(s.line == 1 and s.column == 3)

    assertEq(s.getNextNonWhitespace(skipNoLf), 'c')
    assert(s.line == 1 and s.column == 5)

    assertEq(s.getNextNonWhitespace(skipNoLf), 'd')
    assert(s.line == 1 and s.column == 7)

    assertEq(s.getNextNonWhitespace(skipNoLf), '\l')
    assert(s.line == 2 and s.column == 1)

    assertEq(s.getNextNonWhitespace(skipNoLf), 'e')
    assert(s.line == 2 and s.column == 2)

    assertEq(s.getNextNonWhitespace(skipNoLf), 'f')
    assert(s.line == 2 and s.column == 3)

    assertEq(s.getNextNonWhitespace(skipNoLf), '\0')


  block:
    var s = newParserState(newStringStream("ab c\td # Comment\ne\rf"))

    assert(s.line == 1 and s.column == 1)

    assertEq(s.getNextNonWhitespace(skipLf), 'a')
    assert(s.line == 1 and s.column == 2)

    assertEq(s.getNextNonWhitespace(skipLf), 'b')
    assert(s.line == 1 and s.column == 3)

    assertEq(s.getNextNonWhitespace(skipLf), 'c')
    assert(s.line == 1 and s.column == 5)

    assertEq(s.getNextNonWhitespace(skipLf), 'd')
    assert(s.line == 1 and s.column == 7)

    assertEq(s.getNextNonWhitespace(skipLf), 'e')
    assert(s.line == 2 and s.column == 2)

    assertEq(s.getNextNonWhitespace(skipLf), 'f')
    assert(s.line == 2 and s.column == 3)

    assertEq(s.getNextNonWhitespace(skipLf), '\0')

  # charToInt

  assertEq(charToInt('0', base10), 0)
  assertEq(charToInt('1', base10), 1)
  assertEq(charToInt('2', base10), 2)
  assertEq(charToInt('3', base10), 3)
  assertEq(charToInt('4', base10), 4)
  assertEq(charToInt('5', base10), 5)
  assertEq(charToInt('6', base10), 6)
  assertEq(charToInt('7', base10), 7)
  assertEq(charToInt('8', base10), 8)
  assertEq(charToInt('9', base10), 9)

  assertEq(charToInt('0', base16), 0)
  assertEq(charToInt('1', base16), 1)
  assertEq(charToInt('2', base16), 2)
  assertEq(charToInt('3', base16), 3)
  assertEq(charToInt('4', base16), 4)
  assertEq(charToInt('5', base16), 5)
  assertEq(charToInt('6', base16), 6)
  assertEq(charToInt('7', base16), 7)
  assertEq(charToInt('8', base16), 8)
  assertEq(charToInt('9', base16), 9)
  assertEq(charToInt('a', base16), 10)
  assertEq(charToInt('b', base16), 11)
  assertEq(charToInt('c', base16), 12)
  assertEq(charToInt('d', base16), 13)
  assertEq(charToInt('e', base16), 14)
  assertEq(charToInt('f', base16), 15)
  assertEq(charToInt('A', base16), 10)
  assertEq(charToInt('B', base16), 11)
  assertEq(charToInt('C', base16), 12)
  assertEq(charToInt('D', base16), 13)
  assertEq(charToInt('E', base16), 14)
  assertEq(charToInt('F', base16), 15)

  # parseInt

  block:
    var s = newParserState(newStringStream("1063"))
    assertEq(parseInt(s, base10, LeadingChar.DenyZero), 1063)

  block:
    var s = newParserState(newStringStream("fFa05B"))
    assertEq(parseInt(s, base16, LeadingChar.DenyZero), 16752731)

  block:
    var s = newParserState(newStringStream("01063"))

    try:
      discard parseInt(s, base10, LeadingChar.DenyZero)
      assert false, "An exception should have been raised here!"
    except:
      discard

  block:
    var s = newParserState(newStringStream("01063"))

    assertEq(parseInt(s, base10, LeadingChar.AllowZero), 1063)

  # parseDecimalPart

  block:
    var s = newParserState(newStringStream("24802"))
    # The result should be 0.24802. We check it using integer
    # arithmetic, instead of using the |x - x_expected| < eps...
    assertEq(int(100000 * parseDecimalPart(s)), 24802)


  # parseDateTimePart

  block:
    # We do not include the "YYYY-" part, see the implementation
    # of "praseDateTime" to know why
    var s = newParserState(newStringStream("12-06T11:34:01+13:24"))
    var value: TomlDateTime
    parseDateTimePart(s, value)

    assertEq(value.month, 12)
    assertEq(value.day, 6)
    assertEq(value.hour, 11)
    assertEq(value.minute, 34)
    assertEq(value.second, 01)
    assertEq(value.shift, true)
    assertEq(value.isShiftPositive, true)
    assertEq(value.zoneHourShift, 13)
    assertEq(value.zoneMinuteShift, 24)

  block:
    var s = newParserState(newStringStream("12-06T11:34:01Z"))
    var value: TomlDateTime
    parseDateTimePart(s, value)

    assertEq(value.month, 12)
    assertEq(value.day, 6)
    assertEq(value.hour, 11)
    assertEq(value.minute, 34)
    assertEq(value.second, 01)
    assertEq(value.shift, false)

  block:
    # We do not include the "YYYY-" part, see the implementation
    # of "praseDateTime" to know why
    var s = newParserState(newStringStream("12-06T11:34:01-13:24"))
    var value: TomlDateTime
    parseDateTimePart(s, value)

    assertEq(value.month, 12)
    assertEq(value.day, 6)
    assertEq(value.hour, 11)
    assertEq(value.minute, 34)
    assertEq(value.second, 01)
    assertEq(value.shift, true)
    assertEq(value.isShiftPositive, false)
    assertEq(value.zoneHourShift, 13)
    assertEq(value.zoneMinuteShift, 24)

  # parseSingleLineString

  block:
    var s = newParserState(newStringStream("double string\t\"blahblah"))
    assert parseSingleLineString(s, StringType.Basic) == "double string\t"

  block:
    # Escape sequences
    var s = newParserState(newStringStream('\b' & '\f' & '\l' & '\r' & '\\' &
      '\"' & '\"'))
    assert parseSingleLineString(s, StringType.Basic) == "\b\f\l\r\""

  block:
    # Unicode
    var s = newParserState(newStringStream(r"\u59\U2126\u1f600"""))
    assert parseSingleLineString(s, StringType.Basic) == "YΩ😀"

  # parseMultiLineString

  block:
    var s = newParserState(newStringStream("\ntest\"\"\"blah"))
    # TODO: add tests here
    discard parseMultiLineString(s, StringType.Basic)

  # parseArray

  block:
    var s = newParserState(newStringStream("1, 2, 3, 4]blah"))
    let arr = parseArray(s)

    assertEq(arr.len(), 4)
    assert arr[0].kind == TomlValueKind.Int and arr[0].intVal == 1
    assert arr[1].kind == TomlValueKind.Int and arr[1].intVal == 2
    assert arr[2].kind == TomlValueKind.Int and arr[2].intVal == 3
    assert arr[3].kind == TomlValueKind.Int and arr[3].intVal == 4

  block:
    var s = newParserState(newStringStream("\"a\", \"bb\", \"ccc\"]blah"))
    let arr = parseArray(s)

    assertEq(arr.len(), 3)
    assert arr[0].kind == TomlValueKind.String and arr[0].stringVal == "a"
    assert arr[1].kind == TomlValueKind.String and arr[1].stringVal == "bb"
    assert arr[2].kind == TomlValueKind.String and arr[2].stringVal == "ccc"

  block:
    # Array elements of heterogeneous types are forbidden
    var s = newParserState(newStringStream("1, 2.0, \"foo\"]blah"))

    try:
      discard parseArray(s) # This should raise an exception
      assert false # If we reach this, there's something wrong
    except TomlError:
      discard # That's expected

  # Arrays of tables (they're tricky to implement!)

  try:
    let table = parseString("""
alone = 1

[input]
flags = true

[output]
int_value = 6
str_value = "This is a test"

[deeply.nested]
hello_there = 1.0e+2
""")

    assertEq(table.len(), 4)
    assertEq(table["alone"].kind, TomlValueKind.Int)
    assertEq(table["alone"].intVal, 1)

    block:
      assertEq(table["input"].kind, TomlValueKind.Table)
      let inputTable = table["input"].tableVal
      assertEq(inputTable.len(), 1)
      assertEq(inputTable["flags"].kind, TomlValueKind.Bool)
      assertEq(inputTable["flags"].boolVal, true)

    block:
      assertEq(table["output"].kind, TomlValueKind.Table)
      let outputTable = table["output"].tableVal
      assertEq(outputTable.len(), 2)
      assertEq(outputTable["int_value"].kind, TomlValueKind.Int)
      assertEq(outputTable["int_value"].intVal, 6)
      assertEq(outputTable["str_value"].kind, TomlValueKind.String)
      assertEq(outputTable["str_value"].stringVal, "This is a test")

    block:
      assertEq(table["deeply"].kind, TomlValueKind.Table)
      let deeplyTable = table["deeply"].tableVal
      assertEq(deeplyTable["nested"].kind, TomlValueKind.Table)
      let nestedTable = deeplyTable["nested"].tableVal
      assertEq(nestedTable.len(), 1)
      assertEq(nestedTable["hello_there"].kind, TomlValueKind.Float)
      assertEq(nestedTable["hello_there"].floatVal, 100.0)

  except TomlError:
    let loc = (ref TomlError)(getCurrentException()).location
    echo loc.filename & ":" & $(loc.line) & ":" & $(loc.column) & ":" &
      getCurrentExceptionMsg()

  let fruitTable = parseString("""
[[fruit]]
name = "apple"

[fruit.physical]
  color = "red"
  shape = "round"

[[fruit.variety]]
  name = "red delicious"

[[fruit.variety]]
  name = "granny smith"

[[fruit]]
name = "banana"

[[fruit.variety]]
  name = "plantain"
""")

  assertEq(fruitTable.len(), 1)
  assertEq(fruitTable["fruit"].kind, TomlValueKind.Array)
  assertEq(fruitTable["fruit"].arrayVal[0].kind, TomlValueKind.Table)
  assertEq(fruitTable["fruit"].arrayVal[0].tableVal.len(), 3)
  assertEq(fruitTable["fruit"].arrayVal[0].tableVal["name"].kind,
           TomlValueKind.String)
  assertEq(fruitTable["fruit"].arrayVal[0].tableVal["name"].stringVal, "apple")
  assertEq(fruitTable["fruit"].arrayVal[0].tableVal["physical"].kind,
           TomlValueKind.Table)
  assertEq(fruitTable["fruit"].arrayVal[0].tableVal["variety"].kind,
           TomlValueKind.Array)

  block:
    let varietyTable =
      fruitTable["fruit"].arrayVal[0].tableVal["variety"].arrayVal
    assertEq(varietyTable.len(), 2)
    assertEq(varietyTable[0].kind, TomlValueKind.Table)
    assertEq(varietyTable[0].tableVal["name"].kind, TomlValueKind.String)
    assertEq(varietyTable[0].tableVal["name"].stringVal, "red delicious")
    assertEq(varietyTable[1].kind, TomlValueKind.Table)
    assertEq(varietyTable[1].tableVal["name"].kind, TomlValueKind.String)
    assertEq(varietyTable[1].tableVal["name"].stringVal, "granny smith")

  assertEq(fruitTable["fruit"].arrayVal[1].kind, TomlValueKind.Table)
  assertEq(fruitTable["fruit"].arrayVal[1].tableVal.len(), 2)

  assertEq(fruitTable["fruit"].arrayVal[1].tableVal["name"].kind,
           TomlValueKind.String)
  assertEq(fruitTable["fruit"].arrayVal[1].tableVal["name"].stringVal, "banana")
  assertEq(fruitTable["fruit"].arrayVal[1].tableVal["variety"].kind,
           TomlValueKind.Array)

  block:
    let varietyTable =
      fruitTable["fruit"].arrayVal[1].tableVal["variety"].arrayVal
    assertEq(varietyTable.len(), 1)
    assertEq(varietyTable[0].kind, TomlValueKind.Table)
    assertEq(varietyTable[0].tableVal["name"].kind, TomlValueKind.String)
    assertEq(varietyTable[0].tableVal["name"].stringVal, "plantain")


  # getValueFromFullAddr

  block:
    let node = getValueFromFullAddr(fruitTable, "fruit[1].variety[0].name")
    assertEq(node.kind, TomlValueKind.String)
    assertEq(node.stringVal, "plantain")

  block:
    # Wrong index
    let node = getValueFromFullAddr(fruitTable, "fruit[3]")
    assertEq(node.kind, TomlValueKind.None)

  block:
    # Dangling dot
    let node = getValueFromFullAddr(fruitTable, "fruit[0].")
    assertEq(node.kind, TomlValueKind.None)

  # getString

  assertEq(fruitTable.getString("fruit[0].name"), "apple")
  assertEq(fruitTable.getString("fruit[0].physical.shape"), "round")

  try:
    assertEq(fruitTable.getString("fruit[0].this_does_not_exist"), "")
    assert false, "We should have never reached this line!"
  except:
    discard

  assertEq(fruitTable.getString("fruit[0].color", "yellow"), "yellow")

  # get??Array

  let arrayTable = parseString("""
empty = []
intArr = [1, 2, 3, 4, 5]
floatArr = [10.0, 11.0, 12.0, 13.0]
boolArr = [false, true]
stringArr = ["foo", "bar", "baz"]
dateTimeArr = [1978-02-07T01:02:03Z]
""")

  template checkGetArrayFunc(keyName: string,
                             funcName: untyped,
                             reference: untyped) =
    let arr = arrayTable.funcName(keyName)

    assertEq(len(arr), len(reference))
    for idx in countup(low(arr), high(arr)):
      assertEq(arr[idx], reference[idx])

  assertEq(len(getIntArray(arrayTable, "empty")), 0)
  assertEq(len(getFloatArray(arrayTable, "empty")), 0)
  assertEq(len(getBoolArray(arrayTable, "empty")), 0)
  assertEq(len(getStringArray(arrayTable, "empty")), 0)
  assertEq(len(getDateTimeArray(arrayTable, "empty")), 0)

  checkGetArrayFunc("intArr", getIntArray, [1, 2, 3, 4, 5])
  checkGetArrayFunc("floatArr", getFloatArray, [10.0, 11.0, 12.0, 13.0])
  checkGetArrayFunc("boolArr", getBoolArray, [false, true])
  checkGetArrayFunc("stringArr", getStringArray, ["foo", "bar", "baz"])

  block:
    let referenceDate = TomlDateTime(year: 1978, month: 2, day: 7,
                                     hour: 1, minute: 2, second: 3,
                                     shift: false)

    let arr = arrayTable.getDateTimeArray("dateTimeArr")
    assertEq(len(arr), 1)
    assertEq(arr[0].year, referenceDate.year)
    assertEq(arr[0].month, referenceDate.month)
    assertEq(arr[0].day, referenceDate.day)
    assertEq(arr[0].hour, referenceDate.hour)
    assertEq(arr[0].minute, referenceDate.minute)
    assertEq(arr[0].second, referenceDate.second)
    assertEq(arr[0].shift, referenceDate.shift)
