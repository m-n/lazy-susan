LAZY-SUSAN
==========

Just as a lazy susan is a more flexible and convenient partial table
which rests on top of a traditional table, LAZY-SUSAN is a flexible
and convenient partial reimplementation of Common Lisp's readtables
which sits atop them.

__Status__: Experimental, fun.

__Package Nickname__: ls

Why?
----

For fun and eduction, and to make the following easy
to use:

1. Package local nicknames
2. Synonym symbols.
3. Redefinition of constituent traits
   (e.g. treat \#\\: as something other than a package separator.)
4. Introduction of a new constituent trait: the DIGIT-SEPARATOR can be
   included in arbitrary places within a number without changing how
   the number is read.
5. Customizable treatment of tokens ending in a package marker, allowing
   the user to select one of three options:
   * foo:(bar baz) reads (bar baz) with \*package\* bound to the package designated by foo.
     This is the default behavior of an (LS:RT).
   * foo: returns :foo.
   * error as usual.

LAZY-SUSAN achieves this by implementing the part of the reader
algorithm that is used when collecting a token. To hijack CL's
readtable (rt) machinery we have to use a rt in which our TOKEN-READER
read macro has been set as the macro function for every character that
can start a symbol or number. You can get such a rt -- for visible
ascii characters except period and backslash -- by calling
(LS:RT). (LS:RT) also sets doublequote and \#: macro characters so
they can use our idea of a single escape, and a macro function for
\#B, \#O, \#X, \#R so that they can use digit separators.

Example: Traits
---------------
    (defvar *my-rt* (ls:rt))

    (setf (digit-separators *my-rt*) '(#\_))

    (let ((*readtable* *my-rt*))
      (read-from-string "100_000_000"))

    => (values 100000000 11)

    (let ((*readtable* *my-rt*))
      (read-from-string "#b1000_1000"))

    => (values 136 11)

Example: Package Local Nickname
-------------------------------
    ;;;; package.lisp

    (defpackage #:example
      (:use #:cl))

    (in-package #:example)

    (ls:add-package-local-nickname re cl-ppcre)

    (setf (ls:package-rt 'example) (ls:rt))

    ;;;; example.lisp

    (in-package #:example)

    (ls:in-package/rt #:example)

    (re:scan-to-strings "(\\w+)" "abc def")

The effect of the IN-PACKAGE call above would be redundant and
removed, except that SLIME uses it to determine a buffer's
package. The behavior of SLIME-COMPILE-DEFUN is very confusing when SLIME
incorrectly determines a buffers package, and so we are currently
rethinking this part of LS's interface.

Example: Trailing package marker
--------------------------------

    ;;;; In a context using an (ls:rt)
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setf (trailing-package-marker *readtable*) :keyword))

    (eq :foo foo:)
      -> t

The options are:

    ;; read the following form with *package* bound to the designated package
    (setf (trailing-package-marker *readtable*) :read-form-in-package)

    ;; return a keyword, as seen above
    (setf (trailing-package-marker *readtable*) :keyword)

    ;; signal an error
    (setf (trailing-package-marker *readtable*) ())

Limitations
-----------

The package local nicknames only work for reading symbols, not for
CL:FIND-PACKAGE, CL:PACKAGE-NICKNAMES, and friends.

We have to set the macro function of every character that can start a
token to the LS:TOKEN-READER function to make a LAZY-SUSAN rt.  This
could be heavyweight if we wanted to allow all non-ascii characters to
start tokens.

We don't change printing behavior at all. This can cause print read
inconsistency of strings and symbols if the escape characters are
changed.

SLIME integration is very incomplete. See below and at
https://github.com/m-n/lazy-susan/issues/1 for details.

We haven't tried to include "invalid" syntax yet.

We have so far ignored "potential numbers".

Deliberate Differences
----------------------

By default a when LS reads a token ending in a package marker it will
read the next form with \*PACKAGE\* bound to the package designated by
the token.

Using the default common lisp tokenization, characters can only have one
syntax type, and both macro-character and single-escape are syntax types.
We allow characters to have multiple syntax types. For example, with the
default common lisp reader, if you set-macro-character on \#\\\\ it is no
longer a single escape, and no longer escapes in either symbols or strings.
Using lazy-susan's tokenization and string macro-characters, a macro-character
can still be a single escape where it's macro function is not triggered.

Interface
=========

Package Local Nicknames
-----------------------

Interface burgled or adapted from SBCL

    ADD-PACKAGE-LOCAL-NICKNAME:  Add a package local nickname at eval-always time.
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
    COLLECT-TOKEN:      Collects the next token as (values package-token name-token saw-escape-p package-markers-seen)
    IN-PACKAGE/RT:         IN-PACKAGE alternative. Also sets *readtable*.
    (SETF PACKAGE-RT):  Set a package's default readtable for use with in-package/rt.
    SETUP-PACKAGE-RT:   Set package's default *readtable* to a modified copy of readtable-expression.

Syntax Setfs
------------
    DECIMAL-POINTS:     Within readtable, represent trait by characters.
    DIGIT-SEPARATORS:   Within readtable, represent trait by characters.
    EXPONENT-MARKERS:   Within readtable, represent trait by characters.
    MINUS-SIGNS:        Within readtable, represent trait by characters.
    MULTIPLE-ESCAPES:   Within readtable, represent trait by characters.
    PACKAGE-MARKERS:    Within readtable, represent trait by characters.
    PLUS-SIGNS:         Within readtable, represent trait by characters.
    RATIO-MARKERS:      Within readtable, represent trait by characters.
    SINGLE-ESCAPES:     Within readtable, represent trait by characters.
    WHITESPACES:        Within readtable, represent trait by characters.
    TRAILING-PACKAGE-MARKER: The behavior of the readtable when finding a trailing package marker.

SLIME Integration
=================

SETUP-PACKAGE-RT and (SETF PACKAGE-RT) associate the package to the
readtable on SLIME's SWANK:\*READTABLE-ALIST\*. However, symbol
completion and argument list hinting are currently lost when using
LS's package local nicknames, and we have to start the file with
IN-PACKAGE instead of LS:IN-PACKAGE/RT for SLIME to correctly determine
the buffer's package (which is important for at least SLIME-COMPILE-DEFUN).
