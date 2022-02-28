.. _regexps:

Regular Expression
==================

Regular expressions are the patterns that Alex uses to match tokens in the input stream.

.. _regexp-syntax:

Syntax of regular expressions
-----------------------------

::

   regexp  := rexp2 { '|' rexp2 }

   rexp2   := rexp1 { rexp1 }

   rexp1   := rexp0 [ '*' | '+' | '?' | repeat ]

   rexp0   := set
            | @rmac
            | @string
            | '(' [ regexp ] ')'

   repeat  := '{' $digit+ '}'
            | '{' $digit+ ',' '}'
            | '{' $digit+ ',' $digit+ '}'

The syntax of regular expressions is fairly standard,
the only difference from normal lex-style regular expressions being that we allow the sequence ``()`` to denote the regular expression that matches the empty string.

Spaces are ignored in a regular expression,
so feel free to space out your regular expression as much as you like,
even split it over multiple lines and include comments.
Literal whitespace can be included by surrounding it with quotes ``" "``, or by escaping each whitespace character with ``\``.

``set``
   Matches any of the characters in <set>. See :ref:`Syntax of character sets <charsets>` for the syntax of sets.

``@foo``
   Expands to the definition of the appropriate regular expression macro.

``"..."``
   Matches the sequence of characters in the string, in that order.

``r*``
   Matches zero or more occurrences of <r>.

``r+``
   Matches one or more occurrences of <r>.

``r?``
   Matches zero or one occurrences of <r>.

``r{n}``
   Matches <n> occurrences of <r>.

``r{n,}``
   Matches <n> or more occurrences of <r>.

``r{n,m}``
   Matches between <n> and <m> (inclusive) occurrences of <r>.

.. _charsets:

Syntax of character sets
------------------------

Character sets are the fundamental elements in a regular expression.
A character set is a pattern that matches a single character.
The syntax of character sets is as follows:

::

   set     := set '#' set0
           |  set0

   set0    := @char [ '-' @char ]
           | '.'
           |  @smac
           | '[' [^] { set } ']'
           | '~' set0

The various character set constructions are:

``char``
   The simplest character set is a single Unicode character.
   Note that special characters such as ``[`` and ``.`` must be escaped by prefixing them with ``\``
   (see the lexical syntax, :ref:`Lexical syntax <lexical>`, for the list of special characters).

   Certain non-printable characters have special escape sequences.
   These are: ``\a``, ``\b``, ``\f``, ``\n``, ``\r``, ``\t``, and ``\v``.
   Other characters can be represented by using their numerical character values
   (although this may be non-portable):
   ``\x0A`` is equivalent to ``\n``, for example.

   Whitespace characters are ignored;
   to represent a literal space, escape it with ``\``.

``char-char``
   A range of characters can be expressed by separating the characters with a ‘\ ``-``\ ’,
   all the characters with codes in the given range are included in the set.
   Character ranges can also be non-portable.

``.``
   The built-in set ‘\ ``.``\ ’ matches all characters except newline (``\n``).

   Equivalent to the set ``[\x00-\x10ffff] # \n``.

``set0 # set1``
   Matches all the characters in <set0> that are not in <set1>.

``[sets]``
   The union of <sets>.

``[^sets]``
   The complement of the union of the <sets>. Equivalent to
   ‘\ ``. # [sets]``\ ’.

``~set``
   The complement of <set>.
   Equivalent to ‘\ ``. # set``\ ’

A set macro is written as ``$`` followed by an identifier.
There are some builtin character set macros:

``$white``
   Matches all whitespace characters, including newline.

   Equivalent to the set ``[\ \t\n\f\v\r]``.

``$printable``
   Matches all "printable characters".
   Currently this corresponds to Unicode code points 32 to 0x10ffff,
   although strictly speaking there are many non-printable code points in this region.
   In the future Alex may use a more precise definition of ``$printable``.

Character set macros can be defined at the top of the file at the same time as regular expression macros
(see :ref:`Regular Expression <regexps>`).
Here are some example character set macros:

::

   $lls      = a-z                   -- little letters
   $not_lls  = ~a-z                  -- anything but little letters
   $ls_ds    = [a-zA-Z0-9]           -- letters and digits
   $sym      = [ \! \@ \# \$ ]       -- the symbols !, @, #, and $
   $sym_q_nl = [ \' \! \@ \# \$ \n ] -- the above symbols with ' and newline
   $quotable = $printable # \'       -- any graphic character except '
   $del      = \127                  -- ASCII DEL
