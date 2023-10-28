Introduction
============

Alex is a tool for generating lexical analysers, also known as "lexers" and "scanners", in Haskell.
The lexical analysers implement a description of the tokens to be recognised in the form of regular expressions.
It is similar to the tools "lex" and "flex" for C/C++.

Alex takes a description of tokens based on regular expressions and generates a Haskell module containing code for scanning text
efficiently.
Alex is designed to be familiar to existing lex users,
although it does depart from lex in a number of ways.

A sample specification would be the following:

.. code-block:: none

   {
   module Main (main) where
   }

   %wrapper "basic"

   $digit = 0-9            -- digits
   $alpha = [a-zA-Z]       -- alphabetic characters

   tokens :-

     $white+                        ;
     "--".*                         ;
     let                            { \s -> Let }
     in                             { \s -> In }
     $digit+                        { \s -> Int (read s) }
     [\=\+\-\*\/\(\)]               { \s -> Sym (head s) }
     $alpha [$alpha $digit \_ \']*  { \s -> Var s }

   {
   -- Each action has type :: String -> Token

   -- The token type:
   data Token
     = Let
     | In
     | Sym Char
     | Var String
     | Int Int
     deriving (Eq, Show)

   main = do
     s <- getContents
     print (alexScanTokens s)
   }

The first few lines between the ``{`` and ``}`` provide a code scrap (some inlined Haskell code) to be placed directly in the output,
the scrap at the top of the module is normally used to declare the module name for the generated Haskell module, in this case ``Main``.

The next line, ``%wrapper "basic"`` controls what kind of support code Alex should produce along with the basic scanner.
The ``basic`` wrapper selects a scanner that tokenises a ``String`` and returns a list of tokens.
Wrappers are described fully in :ref:`The Interface to an Alex-generated lexer <api>`.

The next two lines define the ``$digit`` and ``$alpha`` macros for use in the token definitions.

The ‘\ ``tokens :-``\ ’ line ends the macro definitions and starts the definition of the scanner.

The scanner is specified as a series of token definitions where each token specification takes the form of

::

   regexp   { code }

The meaning of this rule is
"if the input matches <regexp>, then return <code>".
The code part along with the braces can be replaced by simply ‘\ ``;``\ ’,
meaning that this token should be ignored in the input stream.
As you can see, we've used this to ignore whitespace in our example.

Our scanner is set up so that the actions are all functions with type ``String->Token``.
When the token is matched, the portion of the input stream that it matched is passed to the appropriate action function as a ``String``.

At the bottom of the file we have another code fragment, surrounded by braces ``{ ... }``.
In this fragment, we declare the type of the tokens, and give a ``main`` function that we can use for testing it;
the ``main`` function just tokenises the input and prints the results to standard output.

Alex has kindly provided the following function which we can use to invoke the scanner:

.. code-block:: haskell

   alexScanTokens :: String -> [Token]

Alex arranges for the input stream to be tokenised,
each of the action functions to be passed the appropriate ``String``,
and a list of ``Token``\ s returned as the result.
If the input stream is lazy, the output stream will also be produced lazily [1]_.

We have demonstrated the simplest form of scanner here,
which was selected by the ``%wrapper "basic"`` line near the top of the file.
In general, actions do not have to have type ``String->Token``,
and there's no requirement for the scanner to return a list of tokens.

With this specification in the file ``Tokens.x``,
Alex can be used to generate ``Tokens.hs``:

.. code-block:: sh

   $ alex Tokens.x

If the module needed to be placed in a different file, ``Main.hs`` for example,
then the output filename can be specified using the ``-o`` option:

.. code-block:: sh

   $ alex Tokens.x -o Main.hs

The resulting module is Haskell 98 compatible.
It can also be readily used with a `Happy <https://www.haskell.org/happy/>`__ parser.

.. [1]
   That is, unless you have any patterns that require a long lookahead.
