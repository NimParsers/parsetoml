Introduction
============

This manual describes Parsetoml, a Nim library to parse TOML files.
The library is meant to be compatible with version 0.3.1 of the TOML
specification. It implements a streaming parser, i.e., a parser which
does not hold the whole file to parse in memory but rather reads one
character at a time. The parser outputs a tree data structure based on
the type :nim:object:`TomlTableRef`.

In this section we provide a short overview of the usage of the
library. We will use the following TOML file as an example::

    [files]
    input_file_name = "test.txt"
    output_file_name = "output.txt"

    [[filters]]
    command = "head"
    lines = 5

    [[filters]]
    command = "cut"
    fields = [1,2,4]

The purpose of this TOML file is to specify how to apply certain
filters to an input text file, and where the result should be saved.
It describes the following shell command::

    cat test.txt | head -n 5 |  cut -f 1,2,4 > output.txt


Parsing a TOML file
-------------------

To parse a file, there are a few functions that can be used. We'll use
the most straightforward one, :nim:proc:`parseFile`: it can either
accept a ``File`` object or a string containing the name of the file
to read. Assuming that the name of the TOML file above is
``test.toml``, we can therefore read the file in memory and dump its
internal representation with the following code:

.. code-block:: nim

    import parsetoml

    let data = parsetoml.parseFile("test.toml")
    parsetoml.dump(data)

(For the sake of clarity, we refer to functions from the Parsetoml
library with a full qualification, e.g., ``parsetoml.parseFile``. This
is however not necessary.) The output of the program is the
following::

    files = table
        input_file_name = string("test.txt")
        output_file_name = string("output.txt")
    filters[0] = table
        command = string("head")
        lines = int(5)
    filters[1] = table
        command = string("cut")
        fields = array(int(1)int(2)int(4))

The purpose of the :nim:proc:`dump` function is to write a readable
representation of the tree of nodes created by functions like
:nim:proc:`parseFile`. It is meant primarly for debugging, and it is a
good tool to understand how Parsetoml works internally.


Navigating through the contents of a TOML file
----------------------------------------------

The ``data`` variable has type :nim:object:`TomlTableRef`, which is a
reference to a :nim:object:`TomlTable` object, i.e., to an ordered
table which associates strings (the keys in the TOML file, e.g.,
``input_file_name``) with values. The latter are represented by a
:nim:object:`TomlValueRef` type.

