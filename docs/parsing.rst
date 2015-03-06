Parse functions
===============

In this section we provide a description of the functions in the
Parsetoml library that read a textual representation of a TOML file
and return a tree of nodes.


Data types
----------

.. nim:object:: TomlTableRef = ref TomlTable

    A reference to a :nim:object:`TomlTable`. This is the default
    return value for all the functions that parse text in TOML format.

.. nim:object:: TomlTable = object

    This data type is used by Parsetoml to associate key names with
    TOML values. The library uses a ``OrderedTable`` instead of a
    ``Table``, so that the order of declaration of the keys in the
    TOML file is preserved.

.. nim:object:: TomlValueRef = ref TomlValue

    A reference to a :nim:object:`TomlValue`. Objects of this kind
    populate :nim:object:`TomlTable` objects.

.. nim:object:: TomlValue = object

    The value associated with a key in a TOML file. It is a parametric
    type defined by the following code:

.. code-block:: nim

    TomlValue* = object
        case kind* : TomlValueKind
        of TomlValueKind.None: nil
        of TomlValueKind.Int: intVal* : int64
        of TomlValueKind.Float: floatVal* : float64
        of TomlValueKind.Bool: boolVal* : bool
        of TomlValueKind.Datetime: dateTimeVal* : TomlDateTime
        of TomlValueKind.String: stringVal* : string
        of TomlValueKind.Array: arrayVal* : seq[TomlValueRef]
        of TomlValueKind.Table: tableVal* : TomlTableRef


.. nim:object:: TomlError = object of Exception

    This exception object is used by Parsetoml to signal errors
    happened during the parser of text. It has just one field,
    ``location``, which is of type ParserState and has the following
    public fields:

================ ===================== ===================================================
Field            Type                  Description
================ ===================== ===================================================
``fileName``     ``string``            Name of the file being parsed (might be "")
``line``         ``int``               Number of the line where the error was detected
``column``       ``int``               Number of the column where the error was detected
``stream``       ``streams.Stream``    Input stream
================ ===================== ===================================================


The following example shows how to properly signal an error during the
parsing of a TOML file to the user:

.. code-block:: nim

    try:
        # Parse some TOML file here
    except parsetoml.TomlError:
        # Retrieve information about the location of the error
        let loc = (ref parsetoml.TomlError)(getCurrentException()).location
        # Print a nicely formatted string explaining what went wrong
        echo(loc.fileName & ":" & $(loc.line) & ":" & $(loc.column) 
             & ": " & getCurrentExceptionMsg())


Procedures
----------

The Parsetoml library provides several functions to parse text in TOML
format. Here is an example of application of the
:nim:proc:`parseString` procedure:

.. code-block:: nim

    import parsetoml

    # We define a "Parameters" tuple which is initialized using data
    # from a TOML file.
    type
        Parameters = tuple
            foo : string
            bar : int64

    proc parseParams(tree : TomlTableRef) : Parameters =
        result.foo = tree.getString("input.foo")
        result.bar = tree.getInt("input.bar")

    let tree = parsetoml.parseString("""
    [input]
    foo = "a"
    bar = 14
    """)

    let params = parseParams(tree)
    assert params.foo == "a"
    assert params.bar == 14


.. nim:proc:: proc parseString(tomlStr : string, fileName : string = "") : TomlTableRef

    Assuming that *tomlStr* is a string containing text in TOML
    format, this function parses it and returns a reference to a newly
    created :nim:object:`TomlTable` object.

    Errors in *tomlStr* are signaled by raising exceptions of type
    :nim:object:`TomlError`. The ``location.fileName`` field of the
    exception itself will be set to *fileName*.

.. nim:proc:: proc parseStream(inputStream : streams.Stream, fileName : string = "") : TomlTableRef

    This function is similar to :nim:proc:`parseString`, but it reads
    data from *inputStream*. The stream is parsed while it is being
    read (i.e., the parsing does not have to wait till the whole file
    has been read in memory).

.. nim:proc:: proc parseFile(f : File, fileName : string = "") : TomlTableRef

    The same as :nim:proc:`parseStream`, but this procedure accepts a
    ``File`` instead of a ``streams.Stream``.

.. nim:proc:: proc parseFile(fileName : string) : TomlTableRef

    This is a wrapper to the previous implementation of ``parseFile``:
    it handles the opening/closing of the file named *fileName*
    automatically.
