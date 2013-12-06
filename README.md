LAZY-SUSAN
==========

A more flexible readtable (rt).

__Status__: Experimental, buggy, ugly, fun.

Why?
----

For fun and eduction, and to make the following easy
to use:

1. Package local nicknames
2. Synonym symbols.
3. Redefinition of constituent traits
   (e.g. treat #\\: as something other than a package seperator.)
4. Introduction of a new constituent trait: the DIGIT-SEPERATOR can be
   included in arbitrary places within a number without changing how
   the number is read.

LAZY-SUSAN achieves this by implementing the part of the reader
algorithm that is used when collecting a token. To hijack CL's rt
machinery we have to use a rt in which our TOKEN-READER read macro has
been set as the macro function for every character that can start a
symbol or number. You can get such a rt -- for ascii characters on
lisps that use a superset of ascii -- by calling (lazy-susan:rt).

Example
-------
    (defvar *my-rt* (lazy-susan:rt))

    (setf (digit-seperators *my-rt*) '(#\z))

    (let ((*readtable* *my-rt*))
      (read-from-string "100z000z000"))

    => (values 100000000 11)

Limitations
-----------
We have to set the macro function of every character that can start a
token to the LAZY-SUSAN:TOKEN-READER function to make a lazy-susan rt.
This could be heavyweight if we wanted to allow non-ascii tokens.

In general we haven't thought much about print read consistency. In
particular the escape character in strings is (by the CLHS) any
character with syntax type single-escape, and we haven't made any
attempt to change their print behavior when we add or remove
single-escape characters.

We haven't tried to include "invalid" syntax yet.

We have so far ignored "potential numbers".

Our error reporting is substandard.

Deliberate Differences
----------------------

Using the default common lisp tokenization, characters can only have one
syntax type, and both macro-character and single-escape are syntax types.
We allow characters to have multiple syntax types. For example, with the
default common lisp reader, if you set-macro-character on #\\\\ it is no
longer a single escape, and no longer escapes in either symbols or strings.
Using lazy-susan's tokenization and string macro-characters, a macro-character
can still be a single escape where it's macro function is not triggered.

Interface
=========

Package Local Nicknames
-----------------------
Interface burgled from SBCL

    PACKAGE-LOCAL-NICKNAME:  Add a package local nickname at eval-always time.
    PACKAGE-LOCAL-NICKNAMES: Return the package local nicknames of this package as ((nn . long-name) ...)
    REMOVE-PACKAGE-LOCAL-NICKNAME:     Remove a package local nickname at eval-always time.

Synonym Symbols
---------------
    SYNONYM-SYMBOL:     Set the symbol to read as another symbol at eval-always time.
    CLEAR-SYNONYM-SYMBOLS:   Remove symbol translations from package at eval-always time.

Readtables
----------
    RT:                 Return copy of ReadTable with lazy-susan features enabled. ASCII only.
    TOKEN-READER:       The reader function used to tokenize a symbol or a number.

Readtble Setfs
--------------
    DECIMAL-POINTS:     Within readtable, represent trait by characters.
    DIGIT-SEPERATORS:   Within readtable, represent trait by characters.
    EXPONENT-MARKERS:   Within readtable, represent trait by characters.
    MINUS-SIGNS:        Within readtable, represent trait by characters.
    MULTIPLE-ESCAPES:   Within readtable, represent trait by characters.
    PACKAGE-MARKERS:    Within readtable, represent trait by characters.
    PLUS-SIGNS:         Within readtable, represent trait by characters.
    RATIO-MARKERS:      Within readtable, represent trait by characters.
    SINGLE-ESCAPES:     Within readtable, represent trait by characters.
    WHITESPACES:        Within readtable, represent trait by characters.
