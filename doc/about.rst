
.. _about:

About Alex
==========

Alex can always be obtained from its `home
page <http://www.haskell.org/alex>`__. The latest source code lives in
the `git repository <https://github.com/simonmar/alex>`__ on ``GitHub``.

.. _relnotes-30:

Release Notes for version 3.0
-----------------------------

-  Unicode support (contributed mostly by Jean-Philippe Bernardy, with
   help from Alan Zimmerman).

   -  An Alex lexer now takes a UTF-8 encoded byte sequence as input
      (see :ref:`Unicode and UTF-8 <encoding>`. If you are using the
      "basic" wrapper or one of the other wrappers that takes a Haskell
      ``String`` as input, the string is automatically encoded into
      UTF-8 by Alex. If your input is a ``ByteString``, you are
      responsible for ensuring that the input is UTF-8 encoded. The old
      8-bit behaviour is still available via the ``--latin1`` option.

   -  Alex source files are assumed to be in UTF-8, like Haskell source
      files. The lexer specification can use Unicode characters and
      ranges.

   -  ``alexGetChar`` is renamed to ``alexGetByte`` in the generated
      code.

   -  There is a new option, ``--latin1``, that restores the old
      behaviour.

-  Alex now does DFA minimization, which helps to reduce the size of the
   generated tables, especially for lexers that use Unicode.

.. _relnotes-22:

Release Notes for version 2.2
-----------------------------

-  Cabal 1.2 is now required.

-  ByteString wrappers: use Alex to lex ByteStrings directly.

.. _relnotes-210:

Release Notes for version 2.1.0
-------------------------------

-  Switch to a Cabal build system: you need a recent version of Cabal
   (1.1.6 or later). If you have GHC 6.4.2, then you need to upgrade
   Cabal before building Alex. GHC 6.6 is fine.

-  Slight change in the error semantics: the input returned on error is
   before the erroneous character was read, not after. This helps to
   give better error messages.

.. _relnotes-20:

Release Notes for version 2.0
-----------------------------

Alex has changed a *lot* between versions 1.x and 2.0. The following is
supposed to be an exhaustive list of the changes:

.. _changes-syntax:

Syntax changes
~~~~~~~~~~~~~~

-  Code blocks are now surrounded by ``{...}`` rather than ``%{...%}``.

-  Character-set macros now begin with ‘\ ``$``\ ’ instead of
   ‘\ ``^``\ ’ and have multi-character names.

-  Regular expression macros now begin with ‘\ ``@``\ ’ instead of
   ‘\ ``%``\ ’ and have multi-character names.

-  Macro definitions are no longer surrounded by ``{ ... }``.

-  Rules are now of the form

   ::

      <c1,c2,...>  regex   { code }

   where ``c1``, ``c2`` are startcodes, and ``code`` is an arbitrary
   Haskell expression.

-  Regular expression syntax changes:

   -  ``()`` is the empty regular expression (used to be ‘\ ``$``\ ’)

   -  set complement can now be expressed as ``[^sets]`` (for similarity
      with lex regular expressions).

   -  The ``'abc'`` form is no longer available, use ``[abc]`` instead.

   -  ‘\ ``^``\ ’ and ‘\ ``$``\ ’ have the usual meanings: ‘\ ``^``\ ’
      matches just after a ‘\ ``\n``\ ’, and ‘\ ``$``\ ’ matches just
      before a ‘\ ``\n``\ ’.

   -  ‘\ ``\n``\ ’ is now the escape character, not ‘\ ``^``\ ’.

   -  The form ``"..."`` means the same as the sequence of characters
      inside the quotes, the difference being that special characters do
      not need to be escaped inside ``"..."``.

-  Rules can have arbitrary predicates attached to them. This subsumes
   the previous left-context and right-context facilities (although
   these are still allowed as syntactic sugar).

.. _changes-files:

Changes in the form of an Alex file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Each file can now only define a single grammar. This change was made
   to simplify code generation. Multiple grammars can be simulated using
   startcodes, or split into separate modules.

-  The programmer experience has been simplified, and at the same time
   made more flexible. See
   :ref:`The Interface to an Alex-generated lexer <api>` for details.

-  You no longer need to import the ``Alex`` module.

.. _changes-usage:

Usage changes
~~~~~~~~~~~~~

The command-line syntax is quite different. See :ref:`Invoking Alex <invoking>`.

.. _changes-implementation:

Implementation changes
~~~~~~~~~~~~~~~~~~~~~~

-  A more efficient table representation, coupled with standard
   table-compression techniques, are used to keep the size of the
   generated code down.

-  When compiling a grammar with GHC, the -g switch causes an even
   faster and smaller grammar to be generated.

-  Startcodes are implemented in a different way: each state corresponds
   to a different initial state in the DFA, so the scanner doesn't have
   to check the startcode when it gets to an accept state. This results
   in a larger, but quicker, scanner.

.. _bug-reports:

Reporting bugs in Alex
----------------------

Please report bugs in Alex to simonmar@microsoft.com. There are no
specific mailing lists for the discussion of Alex-related matters, but
such topics should be fine on the `Haskell
Cafe <http://www.haskell.org/mailman/listinfo/haskell-cafe>`__ mailing
list.

License
-------

Copyright (c) 1995-2011, Chris Dornan and Simon Marlow. All rights
reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

-  Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

-  Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

-  Neither the name of the copyright holders, nor the names of the
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
