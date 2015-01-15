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

import math
import streams
import strutils
import tables
import unsigned

type
    TomlValueKind* = enum
        kindNone
        kindInt,
        kindFloat,
        kindBool,
        kindDatetime,
        kindString,
        kindArray,
        kindTable

    TomlDateTime* = object
        year : int
        month : int
        day : int
        hour : int
        minute : int
        second : int
        zoneHourShift : int
        zoneMinuteShift : int

    TomlTable* = OrderedTable[string, TomlValueRef]
    TomlTableRef* = ref TomlTable

    TomlValueRef* = ref TomlValue
    TomlValue* = object
        case kind : TomlValueKind
        of kindNone: nil
        of kindInt: intVal : int64
        of kindFloat: floatVal : float64
        of kindBool: boolVal : bool
        of kindDatetime: datetimeVal : TomlDateTime
        of kindString: stringVal : string
        of kindArray: arrayVal : seq[TomlValueRef]
        of kindTable: tableVal : TomlTableRef

    ParserState = object
        fileName : string
        line : int
        column : int
        pushback : char
        stream : streams.Stream

    TomlError = object of Exception
        location : ParserState

    NumberBase = enum
        base10, base16

    StringType {. pure .} = enum
        Basic,  # Enclosed within double quotation marks
        Literal # Enclosed within single quotation marks

const
    defaultStringCapacity = 256

################################################################################

proc newTomlError(location : ParserState, msg : string) : ref TomlError =
    result = newException(TomlError, msg)
    result.location = location

################################################################################

proc getNextChar(state : var ParserState) : char =
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

################################################################################

proc pushBackChar(state : var ParserState, c : char) {. inline .} =
    state.pushback = c

################################################################################

type
    LfSkipMode = enum
        skipLf, skipNoLf

proc getNextNonWhitespace(state : var ParserState,
                          skip : LfSkipMode) : char =
    # Note: this procedure does *not* consider a newline as a
    # "whitespace". Since newlines are often mandatory in TOML files
    # (e.g. after a key/value specification), we do not want to miss
    # them...

    let whitespaces = (case skip
                       of skipLf: {' ', '\t', '\r', '\l'}
                       of skipNoLf: {' ', '\t', '\r'})

    var nextChar : char
    while true:
        nextChar = state.getNextChar()
        if nextChar == '#':
            # Skip the comment up to the newline, but do not jump over it
            while nextChar != '\l':
                nextChar = state.getNextChar()

        if nextChar notin whitespaces: break

    result = nextChar

################################################################################

proc charToInt(c : char, base : NumberBase) : int {. inline, noSideEffect .} =
    case base
    of base10: result = int(c) - int('0')
    of base16:
        if c in strutils.Digits:
            result = charToInt(c, base10)
        else:
            result = 10 + int(toUpper(c)) - int('A')

################################################################################

proc parseInt(state : var ParserState, base : NumberBase) : int64 =
    var nextChar : char
    var firstPos = true
    var negative = false
    let baseNum = (case base
                   of base10: 10
                   of base16: 16)

    let digits = (case base
                  of base10: strutils.Digits
                  of base16: strutils.HexDigits)

    result = 0
    while true:
        nextChar = state.getNextChar()
        if nextChar in {'+', '-'} and firstPos:
            firstPos = false
            if nextChar == '-': negative = true
            continue

        if nextChar == '0' and firstPos:
            # TOML specifications forbid this
            raise(newTomlError(state,
                               "leading zeroes are not allowed in integers"))

        if nextChar notin digits:
            state.pushBackChar(nextChar)
            break

        result = result * baseNum + charToInt(nextChar, base)
        if result < 0:
            # If "result" is negative, we have just had an overflow
            raise(newTomlError(state,
                               "integer numbers wider than 64 bits not allowed"))

        firstPos = false
    if negative:
        result = -result

################################################################################

proc parseDecimalPart(state : var ParserState) : float64 =
    var nextChar : char
    var invPowerOfTen = 10

    result = 0.0
    while true:
        nextChar = state.getNextChar()
        if nextChar notin strutils.Digits:
            state.pushBackChar(nextChar)
            break

        result = result + (int(nextChar) - int('0')) / invPowerOfTen
        invPowerOfTen *= 10

################################################################################

proc stringDelimiter(kind : StringType) : char {. inline, noSideEffect .} =
    result = (case kind
              of StringType.Basic: '\"'
              of StringType.Literal: '\'')

################################################################################

proc parseUnicode(state : var ParserState) : string =
    let code = parseInt(state, base16)
    if code < 0:
        raise(newTomlError(state, "negative Unicode codepoint"))

    result = newStringOfCap(6)
    if code <= 0x0000007F:
        result.add(cast[char](code and 0x7F))
    elif code <= 0x000007FF:
        result.add(cast[char](((code shr 6) and 0x1F) or 0xC0) &
                   cast[char](((code shr 0) and 0x3F) or 0x80))
    elif code <= 0x0000FFFF:
        result.add(cast[char](((code shr 12) and 0x0F) or 0xE0) &
                   cast[char](((code shr  6) and 0x3F) or 0x80) &
                   cast[char](((code shr  0) and 0x3F) or 0x80))
    elif code <= 0x001FFFFF:
        result.add(cast[char](((code shr 18) and 0x07) or 0xF0) &
                   cast[char](((code shr 12) and 0x3F) or 0x80) &
                   cast[char](((code shr  6) and 0x3F) or 0x80) &
                   cast[char](((code shr  0) and 0x3F) or 0x80))
    elif code <= 0x03FFFFFF:
        result.add(cast[char](((code shr 24) and 0x03) or 0xF8) &
                   cast[char](((code shr 18) and 0x3F) or 0x80) &
                   cast[char](((code shr 12) and 0x3F) or 0x80) &
                   cast[char](((code shr  6) and 0x3F) or 0x80) &
                   cast[char](((code shr  0) and 0x3F) or 0x80))
    elif code <= 0x7FFFFFFF:
        result.add(cast[char](((code shr 30) and 0x03) or 0xF8) &
                   cast[char](((code shr 24) and 0x3F) or 0x80) &
                   cast[char](((code shr 18) and 0x3F) or 0x80) &
                   cast[char](((code shr 12) and 0x3F) or 0x80) &
                   cast[char](((code shr  6) and 0x3F) or 0x80) &
                   cast[char](((code shr  0) and 0x3F) or 0x80))
    else:
        # We shouldn't reach this point: all the cases have been covered above
        assert false


################################################################################

proc parseEscapeChar(state : var ParserState, escape : char) : string =
    case escape
    of 'b': result = "\b"
    of 't': result = "\t"
    of 'n': result = "\n"
    of 'f': result = "\f"
    of 'r': result = "\r"
    of '\'': result = "\'"
    of '\"': result = "\""
    of '/': result = "/"
    of '\\': result = "\\"
    of 'u', 'U':
        result = parseUnicode(state)
    else:
        raise(newTomlError(state,
                           "unknown escape " &
                           "sequence \"\\" & escape & "\""))

################################################################################

proc parseSingleLineString(state : var ParserState, kind : StringType) : string =
    # This procedure parses strings enclosed within single/double
    # quotation marks. It assumes that the quotation mark has already
    # been consumed by the "state" variable, which therefore is ready
    # to read the first character of the string.

    result = newStringOfCap(defaultStringCapacity)

    let delimiter = stringDelimiter(kind)

    var nextChar : char
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

################################################################################

proc parseMultiLineString(state : var ParserState, kind : StringType) : string =
    # This procedure parses strings enclosed within three consecutive
    # sigle/double quotation marks. It assumes that all the quotation
    # marks have already been consumed by the "state" variable, which
    # therefore is ready to read the first character of the string.

    let delimiter = stringDelimiter(kind)
    var isFirstChar = true

    result = newStringOfCap(defaultStringCapacity)
    var nextChar : char
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

################################################################################

proc parseString(state : var ParserState, kind : StringType) : string =
    # This function assumes that "state" has already consumed the
    # first character (either \" or \', which is passed in the
    # "openChar" parameter).

    let delimiter = stringDelimiter(kind)

    var nextChar : char = state.getNextChar()
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

################################################################################

# Forward declaration
proc parseValue(state : var ParserState) : TomlValueRef

proc parseArray(state : var ParserState) : seq[TomlValueRef] =
    # This procedure assumes that "state" has already consumed the '['
    # character

    result = newSeq[TomlValueRef](0)

    while true:
        var nextChar : char = state.getNextNonWhitespace(skipLf)
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

            let oldState = state # Saved for error messages
            let newValue = parseValue(state)

            if len(result) > 0:
                # Check that the type of newValue is compatible with the
                # previous ones
                if newValue.kind != result[low(result)].kind:
                    raise(newTomlError(oldState,
                                       "array members with incompatible types"))

            result.add(newValue)

################################################################################

proc pow10(x : float64, pow : int64) : float64 {. inline .} =
    if pow == 0:
        result = x
        return

    let mulFactor = if pow < 0:
                        0.1'f64
                    else:
                        10.0'f64

    result = x
    for idx in countup(1, abs(pow)):
        result *= mulFactor

proc parseValue(state : var ParserState) : TomlValueRef =
    var nextChar : char

    nextChar = state.getNextNonWhitespace(skipNoLf)
    case nextChar
    of strutils.Digits:
        state.pushBackChar(nextChar)

        # We can either have an integer or a float
        let intPart = parseInt(state, base10)
        nextChar = state.getNextChar()
        if nextChar == '.':
            let decimalPart = parseDecimalPart(state)
            nextChar = state.getNextChar()
            var exponent : int64 = 0
            if nextChar in {'e', 'E'}:
                exponent = parseInt(state, base10)
            else:
                state.pushBackChar(nextChar)

            let value = pow10(float64(intPart) + decimalPart,
                              exponent)
            result = TomlValueRef(kind: kindFloat,
                                  floatVal: value)
        else:
            state.pushBackChar(nextChar)
            result = TomlValueRef(kind: kindInt,
                                  intVal: intPart)

    of 't':
        # Is this "true"?
        let oldState = state # Only used for error messages
        if state.getNextChar() != 'r' or
           state.getNextChar() != 'u' or
           state.getNextChar() != 'e':
            raise(newTomlError(oldState, "unknown identifier"))
        result = TomlValueRef(kind: kindBool, boolVal: true)

    of 'f':
        # Is this "false"?
        let oldState = state # Only used for error messages
        if state.getNextChar() != 'a' or
           state.getNextChar() != 'l' or
           state.getNextChar() != 's' or
           state.getNextChar() != 'e':
            raise(newTomlError(oldState, "unknown identifier"))
        result = TomlValueRef(kind: kindBool, boolVal: false)

    of '\"':
        # A basic string (accepts \ escape codes)
        result = TomlValueRef(kind: kindString,
                              stringVal: parseString(state, StringType.Basic))

    of '\'':
        # A literal string (does not accept \ escape codes)
        result = TomlValueRef(kind: kindString,
                              stringVal: parseString(state, StringType.Literal))

    of '[':
        # An array
        result = TomlValueRef(kind: kindArray,
                              arrayVal: parseArray(state))

    else:
        raise(newTomlError(state,
                           "unexpected character \"" & nextChar & "\""))

################################################################################

proc parseName(state : var ParserState) : string =
    # This parses the name of a key or a table
    result = newStringOfCap(defaultStringCapacity)

    var nextChar : char
    while true:
        nextChar = state.getNextChar()
        case nextChar
        of '=', '#', '.', '[', ']', '\0', ' ':
            # Any of the above characters marks the end of the name
            state.pushBackChar(nextChar)
            break
        else:
            result.add(nextChar)

################################################################################

type
    BracketType {. pure .} = enum
        single, double

proc parseTableName(state : var ParserState,
                    brackets : BracketType) : seq[string] =
    # This code assumes that '[' has already been consumed
    result = newSeq[string](0)

    while true:
        let partName = state.parseName()
        result.add(partName)

        var nextChar = state.getNextChar()
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

################################################################################

type
    TomlKeyValue = tuple[key : string, value : TomlValueRef]

proc parseKeyValuePair(state : var ParserState) : TomlKeyValue =
    result.key = state.parseName()

    var nextChar = state.getNextNonWhitespace(skipNoLf)
    if nextChar != '=':
        raise(newTomlError(state,
                           "key names cannot contain spaces"))

    result.value = state.parseValue()

    # We must check that there is nothing else in this line
    nextChar = state.getNextNonWhitespace(skipNoLf)
    if nextChar != '\l':
        raise(newTomlError(state,
                           "unexpected character \"" & nextChar & "\""))

################################################################################

proc newParserState(s : streams.Stream, fileName : string = "") : ParserState =
    result = ParserState(fileName: fileName, line: 1, column: 1, stream: s)

################################################################################

proc setEmptyTableVal(val : TomlValueRef) =
    val.kind = kindTable
    new(val.tableVal)
    val.tableVal[] = initOrderedTable[string, TomlValueRef]()

################################################################################

proc setArrayVal(val : TomlValueRef, numOfElems : int = 0) =
    val.kind = kindArray
    val.arrayVal = newSeq[TomlValueRef](numOfElems)

################################################################################

proc advanceToNextNestLevel(state : ParserState,
                            curTablePtr : var TomlTableRef, 
                            tableName : string) =

    let target = (curTablePtr[])[tableName]
    case target.kind
    of kindTable:
        curTablePtr = target.tableVal
    of kindArray:
        let arr = target.arrayVal[high(target.arrayVal)]
        if arr.kind != kindTable:
            raise(newTomlError(state, "\"" & tableName & 
                                  "\" elements are not tables"))
        curTablePtr = arr.tableVal
    else:
        raise(newTomlError(state, "\"" & tableName & 
                           "\" is not a table"))

################################################################################

proc createOrAppendTableArrayDef(state : ParserState,
                                 curTablePtr : var TomlTableRef,
                                 tableNames : seq[string]) =

    # This is a table array entry (e.g. "[[entry]]")
    for idx, tableName in tableNames:
        let lastTableInChain = idx == high(tableNames)

        var newValue : TomlValueRef
        if not hasKey(curTablePtr[], tableName):
            # If this element does not exist, create it
            new(newValue)

            # If this is the last name in the chain (e.g.,
            # "c" in "a.b.c"), its value should be an
            # array of tables, otherwise just a table
            if lastTableInChain:
                setArrayVal(newValue, 1)

                new(newValue.arrayVal[0])
                setEmptyTableVal(newValue.arrayVal[0])

                (curTablePtr[])[tableName] = newValue
                curTablePtr = newValue.arrayVal[0].tableVal
            else:
                setEmptyTableVal(newValue)

                # Add the newly created object to the current table
                (curTablePtr[])[tableName] = newValue

                # Update the pointer to the current table
                curTablePtr = newValue.tableVal
        else:
            # The element exissts: is it of the right type?
            let target = (curTablePtr[])[tableName]

            if lastTableInChain:
                if target.kind != kindArray:
                    raise(newTomlError(state, "\"" & tableName &
                                              " is not an array"))

                var newValue : TomlValueRef
                new(newValue)
                setEmptyTableVal(newValue)
                target.arrayVal.add(newValue)
                curTablePtr = newValue.tableVal
            else:
                advanceToNextNestLevel(state, curTablePtr, tableName)

################################################################################

proc createTableDef(state : ParserState,
                    curTablePtr : var TomlTableRef,
                    tableNames : seq[string]) =

    var newValue : TomlValueRef

    # This starts a new table (e.g. "[table]")
    for tableName in tableNames:
        if not hasKey(curTablePtr[], tableName):
            new(newValue)
            setEmptyTableVal(newValue)

            # Add the newly created object to the current table
            (curTablePtr[])[tableName] = newValue

            # Update the pointer to the current table
            curTablePtr = newValue.tableVal
        else:
            advanceToNextNestLevel(state, curTablePtr, tableName)

################################################################################

proc parseStream*(inputStream : streams.Stream,
                  fileName : string = "") : TomlTableRef =
    var state = newParserState(inputStream, fileName)
    new(result)
    result[] = initOrderedTable[string, TomlValueRef]()

    # This pointer will always point to the table that should get new
    # key/value pairs found in the TOML file during parsing
    var curTablePtr = result

    # Unlike "curTablePtr", this pointer never changes: it always
    # points to the uppermost table in the tree
    let baseTable = result

    var nextChar : char
    while true:
        nextChar = state.getNextNonWhitespace(skipLf)
        case nextChar
        of '[':
            # A new section/table begins. We'll have to start again
            # from the uppermost level, so let's rewind curTablePtr to
            # the root node
            curTablePtr = baseTable

            # First, decompose the table name into its part (e.g.,
            # "a.b.c" -> ["a", "b", "c"])
            nextChar = state.getNextChar()
            let isTableArrayDef = nextChar == '['
            var tableNames : seq[string]
            if isTableArrayDef:
                tableNames = state.parseTableName(BracketType.double)
            else:
                state.pushBackChar(nextChar)
                tableNames = state.parseTableName(BracketType.single)

            if isTableArrayDef:
                createOrAppendTableArrayDef(state, curTablePtr, tableNames)
            else:
                createTableDef(state, curTablePtr, tableNames)

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
            let keyValuePair = state.parseKeyValuePair()
            (curTablePtr[])[keyValuePair.key] = keyValuePair.value

################################################################################

proc parseString*(tomlStr : string, fileName : string = "") : TomlTableRef =
    let strStream = newStringStream(tomlStr)
    result = parseStream(strStream, fileName)

################################################################################

proc parseFile*(f : File, fileName : string = "") : TomlTableRef =
    let fStream = newFileStream(f)
    result = parseStream(fStream, fileName)

################################################################################

proc parseFile*(fileName : string) : TomlTableRef =
    let fStream = newFileStream(fileName, fmRead)
    result = parseStream(fStream, fileName)

################################################################################

proc `$`*(val : TomlDateTime) : string =
    result = $val.year & "-" & $val.month & "-" & $val.day

################################################################################

proc `$`*(val : TomlValue) : string =
    case val.kind
    of kindNone:
        result = "none()"
    of kindInt: 
        result = "int(" & $val.intVal & ")"
    of kindFloat: 
        result = "float(" & $val.floatVal & ")"
    of kindBool: 
        result = "boolean(" & $val.boolVal & ")"
    of kindDatetime: 
        result = "datetime(" & $val.datetimeVal & ")"
    of kindString: 
        result = "string(\"" & $val.stringVal & "\")"
    of kindArray: 
        result = "array("
        for elem in val.arrayVal:
            result.add($(elem[]))
        result.add(")")
    of kindTable: 
        result = "table(" & $(len(val.tableVal)) & " elements)"
    
################################################################################

# This function is mostly useful for debugging purposes
proc dump*(table : TomlTableRef, indentLevel : int = 0) =
    let space = repeatStr(indentLevel, " ")
    for key, val in pairs(table):
        if val.kind == kindTable:
            echo space & key & " = table"
            dump(val.tableVal, indentLevel + 4)
        elif val.kind == kindArray and val.arrayVal[0].kind == kindTable:
            for idx, val in val.arrayVal:
                echo space & key & "[" & $idx & "] = table"
                dump(val.tableVal, indentLevel + 4)
        else:
            echo space & key & " = " & $(val[])

################################################################################

proc newNoneValue() : TomlValueRef =
    new(result)
    result.kind = kindNone

proc getValueFromFullAddr(table : TomlTableRef, 
                          fullAddr : string) : TomlValueRef =

    let fieldNames = strutils.split(fullAddr, '.')
    echo strutils.join(fieldNames, ", ")

    var curTable : ref TomlTable = table
    var curNode : TomlValueRef
    for idx, curFieldName in fieldNames:
        let isLast = idx == len(fieldNames)

        curNode = curTable[curFieldName]
        if not isLast:
            if curNode.kind != kindTable:
                return newNoneValue()
            
            curTable = curNode.tableVal

    result = curNode

################################################################################

when isMainModule:

    template assertEq(T1 : expr, T2 : expr) =
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

    ########################################
    # pow10
    assert pow10(5.0, 1) == 50.0
    assert pow10(5.0, 2) == 500.0
    assert pow10(5.0, 3) == 5000.0

    assert pow10(100.0, -1) == 10.0
    assert pow10(100.0, -2) == 1.0

    ########################################
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

    ########################################
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

    ########################################
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

    ########################################
    # parseInt

    block:
        var s = newParserState(newStringStream("1063"))
        assertEq(parseInt(s, base10), 1063)

    block:
        var s = newParserState(newStringStream("fFa05B"))
        assertEq(parseInt(s, base16), 16752731)

    ########################################
    # parseDecimalPart

    block:
        var s = newParserState(newStringStream("24802"))
        # The result should be 0.24802. We check it using integer
        # arithmetic, instead of using the |x - x_expected| < eps...
        assertEq(int(100000 * parseDecimalPart(s)), 24802)

    ########################################
    # parseSingleLineString

    block:
        var s = newParserState(newStringStream("double string\t\"blahblah"))
        assert parseSingleLineString(s, StringType.Basic) == "double string\t"

    block:
        # Escape sequences
        var s = newParserState(newStringStream('\b' & '\f' & '\l' & '\r' & '\\' & '\"' & '\"'))
        assert parseSingleLineString(s, StringType.Basic) == "\b\f\l\r\""

    block:
        # Unicode
        var s = newParserState(newStringStream(r"\u59\U2126\u1f600"""))
        assert parseSingleLineString(s, StringType.Basic) == "YΩ😀"

    ########################################
    # parseMultiLineString

    block:
        var s = newParserState(newStringStream("\ntest\"\"\"blah"))
        # TODO: add tests here
        discard parseMultiLineString(s, StringType.Basic)

    ########################################
    # parseArray

    block:
        var s = newParserState(newStringStream("1, 2, 3, 4]blah"))
        let arr = parseArray(s)

        assertEq(arr.len(), 4)
        assert arr[0].kind == kindInt and arr[0].intVal == 1
        assert arr[1].kind == kindInt and arr[1].intVal == 2
        assert arr[2].kind == kindInt and arr[2].intVal == 3
        assert arr[3].kind == kindInt and arr[3].intVal == 4

    block:
        var s = newParserState(newStringStream("\"a\", \"bb\", \"ccc\"]blah"))
        let arr = parseArray(s)

        assertEq(arr.len(), 3)
        assert arr[0].kind == kindString and arr[0].stringVal == "a"
        assert arr[1].kind == kindString and arr[1].stringVal == "bb"
        assert arr[2].kind == kindString and arr[2].stringVal == "ccc"

    block:
        # Array elements of heterogeneous types are forbidden
        var s = newParserState(newStringStream("1, 2.0, \"foo\"]blah"))

        try:
            discard parseArray(s) # This should raise an exception
            assert false # If we reach this, there's something wrong
        except TomlError:
            discard # That's expected

    ########################################
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
        assertEq(table["alone"].kind, kindInt)
        assertEq(table["alone"].intVal, 1)

        block:
            assertEq(table["input"].kind, kindTable)
            let inputTable = table["input"].tableVal
            assertEq(inputTable.len(), 1)
            assertEq(inputTable["flags"].kind, kindBool)
            assertEq(inputTable["flags"].boolVal, true)

        block:
            assertEq(table["output"].kind, kindTable)
            let outputTable = table["output"].tableVal
            assertEq(outputTable.len(), 2)
            assertEq(outputTable["int_value"].kind, kindInt)
            assertEq(outputTable["int_value"].intVal, 6)
            assertEq(outputTable["str_value"].kind, kindString)
            assertEq(outputTable["str_value"].stringVal, "This is a test")

        block:
            assertEq(table["deeply"].kind, kindTable)
            let deeplyTable = table["deeply"].tableVal
            assertEq(deeplyTable["nested"].kind, kindTable)
            let nestedTable = deeplyTable["nested"].tableVal
            assertEq(nestedTable.len(), 1)
            assertEq(nestedTable["hello_there"].kind, kindFloat)
            assertEq(nestedTable["hello_there"].floatVal, 100.0)

    except TomlError:
        let loc = (ref TomlError)(getCurrentException()).location
        echo loc.filename & ":" & $(loc.line) & ":" & $(loc.column) & ":" & getCurrentExceptionMsg()

    block:
        let table = parseString("""
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

        assertEq(table.len(), 1)
        assertEq(table["fruit"].kind, kindArray)
        assertEq(table["fruit"].arrayVal[0].kind, kindTable)
        assertEq(table["fruit"].arrayVal[0].tableVal.len(), 3)
        assertEq(table["fruit"].arrayVal[0].tableVal["name"].kind, kindString)
        assertEq(table["fruit"].arrayVal[0].tableVal["name"].stringVal, "apple")
        assertEq(table["fruit"].arrayVal[0].tableVal["physical"].kind, kindTable)
        assertEq(table["fruit"].arrayVal[0].tableVal["variety"].kind, kindArray)

        block:
            let varietyTable = table["fruit"].arrayVal[0].tableVal["variety"].arrayVal
            assertEq(varietyTable.len(), 2)
            assertEq(varietyTable[0].kind, kindTable)
            assertEq(varietyTable[0].tableVal["name"].kind, kindString)
            assertEq(varietyTable[0].tableVal["name"].stringVal, "red delicious")
            assertEq(varietyTable[1].kind, kindTable)
            assertEq(varietyTable[1].tableVal["name"].kind, kindString)
            assertEq(varietyTable[1].tableVal["name"].stringVal, "granny smith")

        assertEq(table["fruit"].arrayVal[1].kind, kindTable)
        assertEq(table["fruit"].arrayVal[1].tableVal.len(), 2)

        assertEq(table["fruit"].arrayVal[1].tableVal["name"].kind, kindString)
        assertEq(table["fruit"].arrayVal[1].tableVal["name"].stringVal, "banana")
        assertEq(table["fruit"].arrayVal[1].tableVal["variety"].kind, kindArray)

        block:
            let varietyTable = table["fruit"].arrayVal[1].tableVal["variety"].arrayVal
            assertEq(varietyTable.len(), 1)
            assertEq(varietyTable[0].kind, kindTable)
            assertEq(varietyTable[0].tableVal["name"].kind, kindString)
            assertEq(varietyTable[0].tableVal["name"].stringVal, "plantain")
