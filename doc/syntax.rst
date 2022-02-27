
.. _syntax:

Alex Files
==========

In this section we describe the layout of an Alex lexical specification.

We begin with the lexical syntax; elements of the lexical syntax are
referred to throughout the rest of this documentation, so you may need
to refer back to the following section several times.

.. _lexical:

Lexical syntax
--------------

Alex's lexical syntax is given below. It is written as a set of macro
definitions using Alex's own syntax. These macros are used in the BNF
specification of the syntax later on.

::

   $digit      = [0-9]
   $octdig     = [0-7]
   $hexdig     = [0-9A-Fa-f]
   $special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
   $graphic    = $printable # $white

   @string     = \" ($graphic # \")* \"
   @id         = [A-Za-z][A-Za-z'_]*
   @smac       = '$' id
   @rmac       = '@' id
   @char       = ($graphic # $special) | @escape
   @escape     = '\\' ($printable | 'x' $hexdig+ | 'o' $octdig+ | $digit+)
   @code       = -- curly braces surrounding a Haskell code fragment

.. _alex-files:

Syntax of Alex files
--------------------

In the following description of the Alex syntax, we use an extended form
of BNF, where optional phrases are enclosed in square brackets
(``[ ... ]``), and phrases which may be repeated zero or more times are
enclosed in braces (``{ ... }``). Literal text is enclosed in single
quotes.

An Alex lexical specification is normally placed in a file with a ``.x``
extension. Alex source files are encoded in UTF-8, just like Haskell
source files [2]_.

The overall layout of an Alex file is:

::

   alex := [ @code ] [ wrapper ] [ encoding ] { macrodef } @id ':-' { rule } [ @code ]

The file begins and ends with optional code fragments. These code
fragments are copied verbatim into the generated source file.

At the top of the file, the code fragment is normally used to declare
the module name and some imports, and that is all it should do: don't
declare any functions or types in the top code fragment, because Alex
may need to inject some imports of its own into the generated lexer
code, and it does this by adding them directly after this code fragment
in the output file.

Next comes an optional directives section

The first kind of directive is a specification:

::

   wrapper := '%wrapper' @string

wrappers are described in :ref:`Wrappers  <wrappers>`. This can be followed
by an optional encoding declaration:

::

   encoding := '%encoding' @string

encodings are described in :ref:`Unicode and UTF-8  <encoding>`.

Additionally, you can specify a token type, a typeclass, or an action
type (depending on what wrapper you use):

::

   action type := '%action' @string

::

   token type := '%token' @string

::

   typeclass(es) := '%typeclass' @string

these are described in :ref:`Type Signatures and Typeclasses  <types>`.

.. _macrodefs:

Macro definitions
~~~~~~~~~~~~~~~~~

Next, the lexer specification can contain a series of macro definitions.
There are two kinds of macros, character set macros, which begin with a
``$``, and regular expression macros, which begin with a ``@``. A
character set macro can be used wherever a character set is valid (see
:ref:`Syntax of character sets  <charsets>`), and a regular expression
macro can be used wherever a regular expression is valid
(see :ref:`Regular Expression  <regexps>`).

::

   macrodef  :=  @smac '=' set
              |  @rmac '=' regexp

Rules
~~~~~

The rules are heralded by the sequence ‘\ ``id :-``\ ’ in the file. It
doesn't matter what you use for the identifier, it is just there for
documentation purposes. In fact, it can be omitted, but the ``:-`` must
be left in.

The syntax of rules is as follows:

::

   rule       := [ startcodes ] token
               | startcodes '{' { token } '}'

   token      := [ left_ctx ] regexp [ right_ctx ]  rhs

   rhs        := @code | ';'

Each rule defines one token in the lexical specification. When the input
stream matches the regular expression in a rule, the Alex lexer will
return the value of the expression on the right hand side, which we call
the action. The action can be any Haskell expression. Alex only places
one restriction on actions: all the actions must have the same type.
They can be values in a token type, for example, or possibly operations
in a monad. More about how this all works is in
:ref:`The Interface to an Alex-generated lexer <api>`.

The action may be missing, indicated by replacing it with ‘\ ``;``\ ’,
in which case the token will be skipped in the input stream.

Alex will always find the longest match. For example, if we have a rule
that matches whitespace:

::

   $white+        ;

Then this rule will match as much whitespace at the beginning of the
input stream as it can. Be careful: if we had instead written this rule
as

::

   $white*        ;

then it would also match the empty string, which would mean that Alex
could never fail to match a rule!

When the input stream matches more than one rule, the rule which matches
the longest prefix of the input stream wins. If there are still several
rules which match an equal number of characters, then the rule which
appears earliest in the file wins.

.. _contexts:

Contexts
^^^^^^^^

Alex allows a left and right context to be placed on any rule:

::

   left_ctx   := '^'
               | set '^'

   right_ctx  := '$'
               | '/' regexp
               | '/' @code

The left context matches the character which immediately precedes the
token in the input stream. The character immediately preceding the
beginning of the stream is assumed to be ‘\ ``\n``\ ’. The special
left-context ‘\ ``^``\ ’ is shorthand for ‘\ ``\n^``\ ’.

Right context is rather more general. There are three forms:

``/ regexp``
   This right-context causes the rule to match if and only if it is
   followed in the input stream by text which matches <regexp>.

   NOTE: this should be used sparingly, because it can have a serious
   impact on performance. Any time this rule *could* match, its
   right-context will be checked against the current input stream.

``$``
   Equivalent to ‘\ ``/\n``\ ’.

``/ { ... }``
   This form is called a *predicate* on the rule. The Haskell expression
   inside the curly braces should have type:

   .. code-block:: haskell

      { ... } :: user       -- predicate state
              -> AlexInput  -- input stream before the token
              -> Int        -- length of the token
              -> AlexInput  -- input stream after the token
              -> Bool       -- True <=> accept the token

   Alex will only accept the token as matching if the predicate returns
   ``True``.

   See :ref:`The Interface to an Alex-generated lexer  <api>` for the
   meaning of the ``AlexInput`` type. The ``user`` argument is available
   for passing into the lexer a special state which is used by
   predicates; to give this argument a value, the ``alexScanUser`` entry
   point to the lexer must be used (see :ref:`Basic interface <basic-api>`).

.. _startcodes:

Start codes
^^^^^^^^^^^

Start codes are a way of adding state to a lexical specification, such
that only certain rules will match for a given state.

A startcode is simply an identifier, or the special start code
‘\ ``0``\ ’. Each rule may be given a list of startcodes under which it
applies:

::

   startcode  := @id | '0'
   startcodes := '<' startcode { ',' startcode } '>'

When the lexer is invoked to scan the next token from the input stream,
the start code to use is also specified
(see :ref:`The Interface to an Alex-generated lexer  <api>`).
Only rules that mention this start code are then enabled. Rules which
do not have a list of startcodes are available all the time.

Each distinct start code mentioned in the lexical specification causes a
definition of the same name to be inserted in the generated source file,
whose value is of type ``Int``. For example, if we mentioned startcodes
``foo`` and ``bar`` in the lexical spec, then Alex will create
definitions such as:

.. code-block:: haskell

   foo = 1
   bar = 2

in the output file.

Another way to think of start codes is as a way to define several
different (but possibly overlapping) lexical specifications in a single
file, since each start code corresponds to a different set of rules. In
concrete terms, each start code corresponds to a distinct initial state
in the state machine that Alex derives from the lexical specification.

Here is an example of using startcodes as states, for collecting the
characters inside a string:

::

   <0>      ([^\"] | \n)*  ;
   <0>      \"             { begin string }
   <string> [^\"]          { stringchar }
   <string> \"             { begin 0 }

When it sees a quotation mark, the lexer switches into the ``string``
state and each character thereafter causes a ``stringchar`` action,
until the next quotation mark is found, when we switch back into the
``0`` state again.

From the lexer's point of view, the startcode is just an integer passed
in, which tells it which state to start in. In order to actually use it
as a state, you must have some way for the token actions to specify new
start codes - :ref:`The Interface to an Alex-generated lexer <api>`
describes some ways this can be done. In some applications, it might be
necessary to keep a *stack* of start codes, where at the end of a state
we pop the stack and resume parsing in the previous state. If you want
this functionality, you have to program it yourself.

.. [2]
   Strictly speaking, GHC source files.
