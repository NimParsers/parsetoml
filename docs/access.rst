Accessing keys and values in a parsed TOML tree
===============================================

Once a TOML file has been parsed, the data are available in a
:nim:object:`TomlTableRef` object, which implements a tree-like
structure. The Parsetoml library provides several ways to access the
information encoded in the tree:

1. Generic functions (easy)
2. Tree traversal (complex but powerful)


TOML addresses
--------------

An *address* is a string which identifies the position of a key/value
pair within a TOML file. Parsetoml allows to quickly retrieve the
value of a key given its address, without the need of traversing the
whole TOML tree.

Addresses can reference sub-tables as well as elements of table
arrays: the names of nested sub-tables are separated by dots, while
integer numbers within square brackets indicate elements of a table
array.

As an instance, consider the following TOML table::

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


This TOML file contains an array of tables named ``fruit``; the array
contains two elements: an ``apple`` and a ``banana``. Each element
contains an additional array of tables, named in both cases
``fruit.variety``: the apple element has two varieties, while the
banana element has just one.

The value ``granny smith`` is associated to the key whose address is
``fruit[0].variety[1].name``, while the value ``round`` is associated
to the key ``fruit[0].physical.shape``. Here is a commented version of
the TOML file where each key/value pair has its address spelled
explicitly::

    [[fruit]]
      name = "apple"             # fruit[0].name

      [fruit.physical]
        color = "red"            # fruit[0].physical.color
        shape = "round"          # fruit[0].physical.color

      [[fruit.variety]]
        name = "red delicious"   # fruit[0].variety[0].name

      [[fruit.variety]]
        name = "granny smith"    # fruit[0].variety[1].name

    [[fruit]]
      name = "banana"            # fruit[1].name

      [[fruit.variety]]
        name = "plantain"        # fruit[1].variety[0].name


Generic functions
-----------------

Each of the functions listed in this section returns the value
associated with the key whose address is passed in the parameters
named either *fullAddr* or *address*.

.. nim:proc:: proc getValueFromFullAddr(table : TomlTableRef, fullAddr : string) : TomlValueRef

    Looks for an element in *table* that matches the address
    *fullAddr* and return the corresponding value. If no match is
    found, a :nim:object:`TomlValueRef` object with kind
    :nim:enum:`TomlValueKind` equal to ``None`` is returned.

.. nim:proc:: proc getInt(table : TomlTableRef, address : string) : int64

    Wrapper to :nim:proc::`getValueFromFullAddr`. if *address* points
    to an key that does not exists, or if the type of the key pointed
    by *address* is not an integer, a ``KeyError`` exception is
    raised.

.. nim:proc:: proc getInt(table : TomlTableRef, address : string, default : int64) : int64

    Wrapper to :nim:proc::`getValueFromFullAddr`. if the type of the
    key pointed by *address* is not an integer, a ``KeyError``
    exception is raised. However, if the key does not exist, the value
    of *default* will be returned instead (i.e., no ``KeyError``
    exception is raised).

.. nim:proc:: proc getFloat(table : TomlTableRef, address : string) : float64

    Wrapper to :nim:proc::`getValueFromFullAddr`. if *address* points
    to an key that does not exists, or if the type of the key pointed
    by *address* is not a floating point value, a ``KeyError``
    exception is raised.

.. nim:proc:: proc getFloat(table : TomlTableRef, address : string, default : float64) : float64

    Wrapper to :nim:proc::`getValueFromFullAddr`. if the type of the
    key pointed by *address* is not a floating point value, a
    ``KeyError`` exception is raised. However, if the key does not
    exist, the value of *default* will be returned instead (i.e., no
    ``KeyError`` exception is raised).

.. nim:proc:: proc getBool(table : TomlTableRef, address : string) : bool

    Wrapper to :nim:proc::`getValueFromFullAddr`. if *address* points
    to an key that does not exists, or if the type of the key pointed
    by *address* is not a Boolean, a ``KeyError`` exception is
    raised.

.. nim:proc:: proc getBool(table : TomlTableRef, address : string, default : bool) : bool

    Wrapper to :nim:proc::`getValueFromFullAddr`. if the type of the
    key pointed by *address* is not a Boolean, a ``KeyError``
    exception is raised. However, if the key does not exist, the value
    of *default* will be returned instead (i.e., no ``KeyError``
    exception is raised).

.. nim:proc:: proc getString(table : TomlTableRef, address : string) : string

    Wrapper to :nim:proc::`getValueFromFullAddr`. if *address* points
    to an key that does not exists, or if the type of the key pointed
    by *address* is not a string, a ``KeyError`` exception is raised.

.. nim:proc:: proc getString(table : TomlTableRef, address : string, default : string) : string

    Wrapper to :nim:proc::`getValueFromFullAddr`. if the type of the
    key pointed by *address* is not a string, a ``KeyError`` exception
    is raised. However, if the key does not exist, the value of
    *default* will be returned instead (i.e., no ``KeyError``
    exception is raised).

.. nim:proc:: proc getDateTime(table : TomlTableRef, address : string) : parsetoml.TomlDateTime

    Wrapper to :nim:proc::`getValueFromFullAddr`. if *address* points
    to an key that does not exists, or if the type of the key pointed
    by *address* is not a date/time, a ``KeyError`` exception is
    raised.

.. nim:proc:: proc getDateTime(table : TomlTableRef, address : string, default : parsetoml.TomlDateTime) : parsetoml.TomlDateTime

    Wrapper to :nim:proc::`getValueFromFullAddr`. if the type of the
    key pointed by *address* is not a date/time, a ``KeyError``
    exception is raised. However, if the key does not exist, the value
    of *default* will be returned instead (i.e., no ``KeyError``
    exception is raised).


Tree traversal
--------------

It is possible to directly access the fields of a
:nim:object:`TomlTableRef` to perform a tree traversal of the data
structure. The implementation of the :nim:proc:`dump` procedure is
extremely interesting in this respect:

.. code-block:: nim

    proc dump*(table : TomlTableRef, indentLevel : int = 0) =
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
