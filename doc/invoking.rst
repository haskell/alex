
.. _invoking:

Invoking Alex
=============

The command line syntax for Alex is entirely standard:

::

   $ alex { option } file.x  { option }

Alex expects a single ``file.x`` to be named on the command line. By
default, Alex will create ``file.hs`` containing the Haskell source for
the lexer.

The options that Alex accepts are listed below:

``-o`` <file>; ``--outfile``\ =<file>
   Specifies the filename in which the output is to be placed. By
   default, this is the name of the input file with the ``.x`` suffix
   replaced by ``.hs``.

``-i`` [<file>]; ``--info`` [<=file>]
   Produces a human-readable rendition of the state machine (DFA) that
   Alex derives from the lexer, in <file> (default: ``file.info`` where
   the input file is ``file.x``).

   The format of the info file is currently a bit basic, and not
   particularly informative.

``-t`` [<dir>]; ``--template``\ =<dir>
   Look in <dir> for template files.

``-g``; ``--ghc``
   Causes Alex to produce a lexer which is optimised for compiling with
   GHC. The lexer will be significantly more efficient, both in terms of
   the size of the compiled lexer and its runtime.

``-d``; ``--debug``
   Causes Alex to produce a lexer which will output debugging messages
   as it runs.

``-l``; ``--latin1``
   Disables the use of UTF-8 encoding in the generated lexer. This has
   two consequences:

   -  The Alex source file is still assumed to be UTF-8 encoded, but any
      Unicode characters outside the range 0-255 are mapped to Latin-1
      characters by taking the code point modulo 256.

   -  The built-in macros ``$printable`` and '``.``' range over the
      Latin-1 character set, not the Unicode character set.

   Note that this currently does not disable the UTF-8 encoding that
   happens in the "basic" wrappers, so ``--latin1`` does not make sense
   in conjunction with these wrappers (not that you would want to do
   that, anyway). Alternatively, a ``%encoding "latin1"`` declaration
   can be used inside the Alex source file to request a Latin-1 mapping.
   See also :ref:`Unicode and UTF-8 <encoding>` for more information about
   the ``%encoding`` declaration.

``-?``; ``--help``
   Display help and exit.

``-V``; ``--version``
   Output version information and exit. Note that for legacy reasons
   ``-v`` is supported, too, but the use of it is deprecated. ``-v``
   will be used for verbose mode when it is actually implemented.
