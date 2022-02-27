
.. _api:

The Interface to an Alex-generated lexer
========================================

This section answers the question: "How do I include an Alex lexer in my
program?"

Alex provides for a great deal of flexibility in how the lexer is
exposed to the rest of the program. For instance, there's no need to
parse a ``String`` directly if you have some special character-buffer
operations that avoid the overheads of ordinary Haskell ``String``\ s.
You might want Alex to keep track of the line and column number in the
input text, or you might wish to do it yourself (perhaps you use a
different tab width from the standard 8-columns, for example).

The general story is this: Alex provides a basic interface to the
generated lexer (described in the next section), which you can use to
parse tokens given an abstract input type with operations over it. You
also have the option of including a wrapper, which provides a
higher-level abstraction over the basic interface; Alex comes with
several wrappers.

.. _encoding:

Unicode and UTF-8
-----------------

Lexer specifications are written in terms of Unicode characters, but
Alex works internally on a UTF-8 encoded byte sequence.

Depending on how you use Alex, the fact that Alex uses UTF-8 encoding
internally may or may not affect you. If you use one of the wrappers
(below) that takes input from a Haskell ``String``, then the UTF-8
encoding is handled automatically. However, if you take input from a
``ByteString``, then it is your responsibility to ensure that the input
is properly UTF-8 encoded.

None of this applies if you used the ``--latin1`` option to Alex or
specify a Latin-1 encoding via a ``%encoding`` declaration. In that
case, the input is just a sequence of 8-bit bytes, interpreted as
characters in the Latin-1 character set.

The following (case-insenstive) encoding strings are currently
supported:

``%encoding "latin-1"``; ``%encoding "iso-8859-1"``
   Declare Latin-1 encoding as described above.

``%encoding "utf-8"``; ``%encoding "utf8"``
   Declare UTF-8 encoding. This is the default encoding but it may be
   useful to explicitly declare this to make protect against Alex being
   called with the ``--latin1`` flag.

.. _basic-api:

Basic interface
---------------

If you compile your Alex file without a ``%wrapper`` declaration, then
you get access to the lowest-level API to the lexer. You must provide
definitions for the following, either in the same module or imported
from another module:

.. code-block:: haskell

   type AlexInput
   alexGetByte       :: AlexInput -> Maybe (Word8,AlexInput)
   alexInputPrevChar :: AlexInput -> Char

The generated lexer is independent of the input type, which is why you
have to provide a definition for the input type yourself. Note that the
input type needs to keep track of the *previous* character in the input
stream; this is used for implementing patterns with a left-context
(those that begin with ``^`` or ``set^``). If you don't ever use
patterns with a left-context in your lexical specification, then you can
safely forget about the previous character in the input stream, and have
``alexInputPrevChar`` return ``undefined``.

Alex will provide the following function:

.. code-block:: haskell

   alexScan :: AlexInput             -- The current input
            -> Int                   -- The "start code"
            -> AlexReturn action     -- The return value

   data AlexReturn action
     = AlexEOF

     | AlexError
         !AlexInput     -- Remaining input

     | AlexSkip
         !AlexInput     -- Remaining input
         !Int           -- Token length

     | AlexToken
         !AlexInput     -- Remaining input
         !Int           -- Token length
         action         -- action value

Calling ``alexScan`` will scan a single token from the input stream, and
return a value of type ``AlexReturn``. The value returned is either:

``AlexEOF``
   The end-of-file was reached.

``AlexError``
   A valid token could not be recognised.

``AlexSkip``
   The matched token did not have an action associated with it.

``AlexToken``
   A token was matched, and the action associated with it is returned.

The ``action`` is simply the value of the expression inside ``{...}`` on
the right-hand-side of the appropriate rule in the Alex file. Alex
doesn't specify what type these expressions should have, it simply
requires that they all have the same type, or else you'll get a type
error when you try to compile the generated lexer.

Once you have the ``action``, it is up to you what to do with it. The
type of ``action`` could be a function which takes the ``String``
representation of the token and returns a value in some token type, or
it could be a continuation that takes the new input and calls
``alexScan`` again, building a list of tokens as it goes.

This is pretty low-level stuff; you have complete flexibility about how
you use the lexer, but there might be a fair amount of support code to
write before you can actually use it. For this reason, we also provide a
selection of wrappers that add some common functionality to this basic
scheme. Wrappers are described in the next section.

There is another entry point, which is useful if your grammar contains
any predicates (see :ref:`Contexts <contexts>`):

.. code-block:: haskell

   alexScanUser
            :: user             -- predicate state
            -> AlexInput        -- The current input
            -> Int              -- The "start code"
            -> AlexReturn action

The extra argument, of some type ``user``, is passed to each predicate.

.. _wrappers:

Wrappers
--------

To use one of the provided wrappers, include the following declaration
in your file:

::

   %wrapper "name"

where <name> is the name of the wrapper, eg. ``basic``. The following
sections describe each of the wrappers that come with Alex.

The "basic" wrapper
~~~~~~~~~~~~~~~~~~~

The basic wrapper is a good way to obtain a function of type
``String -> [token]`` from a lexer specification, with little fuss.

It provides definitions for ``AlexInput``, ``alexGetByte`` and
``alexInputPrevChar`` that are suitable for lexing a ``String`` input.
It also provides a function ``alexScanTokens`` which takes a ``String``
input and returns a list of the tokens it contains.

The ``basic`` wrapper provides no support for using startcodes; the
initial startcode is always set to zero.

Here is the actual code included in the lexer when the basic wrapper is
selected:

.. code-block:: haskell

   type AlexInput =
     ( Char      -- previous char
     , [Byte]    -- rest of the bytes for the current char
     , String    -- rest of the input string
     )

   alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
   alexGetByte (c, b:bs, s  ) = Just (b, (c, bs, s))
   alexGetByte (c, []  , [] ) = Nothing
   alexGetByte (_, []  , c:s) = case utf8Encode c of
                                  b:bs -> Just (b, (c, bs, s))

   alexInputPrevChar :: AlexInput -> Char
   alexInputPrevChar (c, _, _) = c

   -- alexScanTokens :: String -> [token]
   alexScanTokens str = go ('\n', [], str)
     where
       go inp@(_,_bs,str) =
         case alexScan inp 0 of
           AlexEOF                -> []
           AlexSkip  inp' len     -> go inp'
           AlexToken inp' len act -> act (take len str) : go inp'
           AlexError _            -> error "lexical error"

The type signature for ``alexScanTokens`` is commented out, because the
``token`` type is unknown. All of the actions in your lexical
specification should have type:

.. code-block:: haskell

   { ... } :: String -> token

for some type ``token``.

For an example of the use of the basic wrapper, see the file
``examples/Tokens.x`` in the Alex distribution.

The "posn" wrapper
~~~~~~~~~~~~~~~~~~

The posn wrapper provides slightly more functionality than the basic
wrapper: it keeps track of line and column numbers of tokens in the
input text.

The posn wrapper provides the following, in addition to the
straightforward definitions of ``alexGetByte`` and
``alexInputPrevChar``:

.. code-block:: haskell

   data AlexPosn = AlexPn
     !Int            -- absolute character offset
     !Int            -- line number
     !Int            -- column number

   type AlexInput =
     ( AlexPosn      -- current position,
     , Char          -- previous char
     , [Byte]        -- rest of the bytes for the current char
     , String        -- current input string
     )

   -- alexScanTokens :: String -> [token]
   alexScanTokens str = go (alexStartPos, '\n', [], str)
     where
       go inp@(pos, _, _, str) =
         case alexScan inp 0 of
           AlexEOF                -> []
           AlexSkip  inp' len     -> go inp'
           AlexToken inp' len act -> act pos (take len str) : go inp'
           AlexError (AlexPn _ line column, _, _, _) -> error $ unwords
             [ "lexical error at", show line, "line,", show column, "column" ]

The types of the token actions should be:

.. code-block:: haskell

   { ... } :: AlexPosn -> String -> token

For an example using the ``posn`` wrapper, see the file
``examples/Tokens_posn.x`` in the Alex distribution.

The "monad" wrapper
~~~~~~~~~~~~~~~~~~~

The ``monad`` wrapper is the most flexible of the wrappers provided with
Alex. It includes a state monad which keeps track of the current input
and text position, and the startcode. It is intended to be a template
for building your own monads - feel free to copy the code and modify it
to build a monad with the facilities you need.

.. code-block:: haskell

   data AlexState = AlexState
     { alex_pos   :: !AlexPosn  -- position at current input location
     , alex_inp   :: String     -- the current input
     , alex_chr   :: !Char      -- the character before the input
     , alex_bytes :: [Byte]     -- rest of the bytes for the current char
     , alex_scd   :: !Int       -- the current startcode
     }

   newtype Alex a = Alex { unAlex :: AlexState
                                  -> Either String (AlexState, a) }

   instance Functor     Alex where ...
   instance Applicative Alex where ...
   instance Monad       Alex where ...

   runAlex          :: String -> Alex a -> Either String a

   type AlexInput =
     ( AlexPosn                 -- current position,
     , Char                     -- previous char
     , [Byte]                   -- rest of the bytes for the current char
     , String                   -- current input string
     )

   alexGetInput     :: Alex AlexInput
   alexSetInput     :: AlexInput -> Alex ()

   alexError        :: String -> Alex a

   alexGetStartCode :: Alex Int
   alexSetStartCode :: Int -> Alex ()

The ``monad`` wrapper expects that you define a variable ``alexEOF``
with the following signature:

.. code-block:: haskell

   alexEOF :: Alex result

To invoke a scanner under the ``monad`` wrapper, use ``alexMonadScan``:

.. code-block:: haskell

   alexMonadScan :: Alex result

The token actions should have the following type:

.. code-block:: haskell

   type AlexAction result = AlexInput -> Int -> Alex result
   { ... }  :: AlexAction result

The Alex file must also define a function ``alexEOF``, which will be
executed on when the end-of-file is scanned:

.. code-block:: haskell

   alexEOF :: Alex result

The ``monad`` wrapper also provides some useful combinators for
constructing token actions:

.. code-block:: haskell

   -- skip :: AlexAction result
   skip input len = alexMonadScan

   -- andBegin :: AlexAction result -> Int -> AlexAction result
   (act `andBegin` code) input len = do alexSetStartCode code; act input len

   -- begin :: Int -> AlexAction result
   begin code = skip `andBegin` code

   -- token :: (AlexInput -> Int -> token) -> AlexAction token
   token t input len = return (t input len)

The "monadUserState" wrapper
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``monadUserState`` wrapper is built upon the ``monad`` wrapper. It
includes a reference to a type which must be defined in the user's
program, ``AlexUserState``, and a call to an initialization function
which must also be defined in the user's program, ``alexInitUserState``.
It gives great flexibility because it is now possible to add any needed
information and carry it during the whole lexing phase.

The generated code is the same as in the ``monad`` wrapper, except in 3
places:

1. The definition of the general state, which now refers to a type
   ``AlexUserState`` that must be defined in the Alex file.

   .. code-block:: haskell

      data AlexState = AlexState
        { alex_pos   :: !AlexPosn      -- position at current input location
        , alex_inp   :: String         -- the current input
        , alex_chr   :: !Char          -- the character before the input
        , alex_bytes :: [Byte]         -- rest of the bytes for the current char
        , alex_scd   :: !Int           -- the current startcode
        , alex_ust   :: AlexUserState  -- AlexUserState will be defined in the user program
        }

2. The initialization code, where a user-specified routine
   (``alexInitUserState``) will be called.

   .. code-block:: haskell

      runAlex :: String -> Alex a -> Either String a
      runAlex input (Alex f) = case f st of
          Left msg     -> Left msg
          Right (_, a) -> Right a
        where
          st = AlexState
               { alex_pos   = alexStartPos
               , alex_inp   = input
               , alex_chr   = '\n'
               , alex_bytes = []
               , alex_ust   = alexInitUserState
               , alex_scd   = 0
               }

3. Two helper functions (``alexGetUserState`` and ``alexSetUserState``)
   are defined.

   .. code-block:: haskell

      alexGetUserState :: Alex AlexUserState
      alexSetUserState :: AlexUserState -> Alex ()

Here is an example of code in the user's Alex file defining the type and
function:

.. code-block:: haskell

   data AlexUserState = AlexUserState
     { lexerCommentDepth  :: Int
     , lexerStringValue   :: String
     }

   alexInitUserState :: AlexUserState
   alexInitUserState = AlexUserState
     { lexerCommentDepth  = 0
     , lexerStringValue   = ""
     }

   getLexerCommentDepth :: Alex Int
   getLexerCommentDepth = lexerCommentDepth <$> alexGetUserState

   setLexerCommentDepth :: Int -> Alex ()
   setLexerCommentDepth ss = do
     ust <- alexGetUserState
     alexSetUserState ust{ lexerCommentDepth = ss }

   getLexerStringValue :: Alex String
   getLexerStringValue = lexerStringValue <$> alexGetUserState

   setLexerStringValue :: String -> Alex ()
   setLexerStringValue ss = do
     ust <- alexGetUserState
     alexSetUserState ust{ lexerStringValue = ss }

   addCharToLexerStringValue :: Char -> Alex ()
   addCharToLexerStringValue c = do
     ust <- alexGetUserState
     alexSetUserState ust{ lexerStringValue = c : lexerStringValue ust }

The "gscan" wrapper
~~~~~~~~~~~~~~~~~~~

The ``gscan`` wrapper is provided mainly for historical reasons: it
exposes an interface which is very similar to that provided by Alex
version 1.x. The interface is intended to be very general, allowing
actions to modify the startcode, and pass around an arbitrary state
value.

.. code-block:: haskell

   alexGScan :: StopAction state result -> state -> String -> result

   type StopAction state result =
     AlexPosn -> Char -> String -> (Int, state) -> result

The token actions should all have this type:

.. code-block:: haskell

   { ... }   :: AlexPosn                  -- token position
             -> Char                      -- previous character
             -> String                    -- input string at token
             -> Int                       -- length of token
             -> ((Int, state) -> result)  -- continuation
             -> (Int, state)              -- current (startcode, state)
             -> result

The bytestring wrappers
~~~~~~~~~~~~~~~~~~~~~~~

The ``basic-bytestring``, ``posn-bytestring`` and ``monad-bytestring``
wrappers are variations on the ``basic``, ``posn`` and ``monad``
wrappers that use lazy ``ByteString``\ s as the input and token types
instead of an ordinary ``String``.

The point of using these wrappers is that ``ByteString``\ s provide a
more memory efficient representation of an input stream. They can also
be somewhat faster to process. Note that using these wrappers adds a
dependency on the ``ByteString`` modules, which live in the
``bytestring`` package (or in the ``base`` package in ``ghc-6.6``)

As mentioned earlier (:ref:`Unicode and UTF-8 <encoding>`), Alex lexers
internally process a UTF-8 encoded string of bytes. This means that the
``ByteString`` supplied as input when using one of the ByteString
wrappers should be UTF-8 encoded (or use either the ``--latin1`` option
or the ``%encoding`` declaration).

Do note that ``token`` provides a *lazy* ``ByteString`` which is not the
most compact representation for short strings. You may want to convert
to a strict ``ByteString`` or perhaps something more compact still. Note
also that by default tokens share space with the input ``ByteString``
which has the advantage that it does not need to make a copy but it also
prevents the input from being garbage collected. It may make sense in
some applications to use ``ByteString``'s ``copy`` function to unshare
tokens that will be kept for a long time, to allow the original input to
be collected.

The "basic-bytestring" wrapper
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``basic-bytestring`` wrapper is the same as the ``basic`` wrapper
but with lazy ``ByteString`` instead of ``String``:

.. code-block:: haskell

   import           Data.ByteString.Lazy (ByteString)
   import qualified Data.ByteString.Lazy as ByteString

   data AlexInput = AlexInput
     { alexChar    :: {-# UNPACK #-} !Char    -- previous char
     , alexStr     :: !ByteString             -- current input string
     , alexBytePos :: {-# UNPACK #-} !Int64   -- bytes consumed so far
     }

   alexGetByte       :: AlexInput -> Maybe (Char, AlexInput)

   alexInputPrevChar :: AlexInput -> Char

   -- alexScanTokens :: ByteString -> [token]

All of the actions in your lexical specification should have type:

.. code-block:: haskell

   { ... } :: ByteString -> token

for some type ``token``.

The "posn-bytestring" wrapper
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``posn-bytestring`` wrapper is the same as the ``posn`` wrapper but
with lazy ``ByteString`` instead of ``String``:

.. code-block:: haskell

   import           Data.ByteString.Lazy (ByteString)
   import qualified Data.ByteString.Lazy as ByteString

   type AlexInput =
     ( AlexPosn    -- current position
     , Char        -- previous char
     , ByteString  -- current input string
     , Int64       -- bytes consumed so far
     )

   -- alexScanTokens :: ByteString -> [token]

All of the actions in your lexical specification should have type:

.. code-block:: haskell

   { ... } :: AlexPosn -> ByteString -> token

for some type ``token``.

The "monad-bytestring" wrapper
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``monad-bytestring`` wrapper is the same as the ``monad`` wrapper
but with lazy ``ByteString`` instead of ``String``:

.. code-block:: haskell

   import           Data.ByteString.Lazy (ByteString)
   import qualified Data.ByteString.Lazy as ByteString

   data AlexState = AlexState
     { alex_pos  :: !AlexPosn   -- position at current input location
     , alex_bpos :: !Int64      -- bytes consumed so far
     , alex_inp  :: ByteString  -- the current input
     , alex_chr  :: !Char       -- the character before the input
     , alex_scd  :: !Int        -- the current startcode
     }

   newtype Alex a = Alex { unAlex :: AlexState
                                  -> Either String (AlexState, a) }

   runAlex :: ByteString -> Alex a -> Either String a

   type AlexInput =
     ( AlexPosn                 -- current position
     , Char                     -- previous char
     , ByteString               -- current input string
     , Int64                    -- bytes consumed so far
     )

   -- token :: (AlexInput -> Int -> token) -> AlexAction token

All of the actions in your lexical specification have the same type as
in the ``monad`` wrapper. It is only the types of the function to run
the monad and the type of the ``token`` function that change.

The "monadUserState-bytestring" wrapper
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``monadUserState-bytestring`` wrapper is the same as the
``monadUserState`` wrapper but with lazy ``ByteString`` instead of
``String``:

.. code-block:: haskell

   import           Data.ByteString.Lazy (ByteString)
   import qualified Data.ByteString.Lazy as ByteString

   data AlexState = AlexState
     { alex_pos  :: !AlexPosn      -- position at current input location
     , alex_bpos :: !Int64         -- bytes consumed so far
     , alex_inp  :: ByteString     -- the current input
     , alex_chr  :: !Char          -- the character before the input
     , alex_scd  :: !Int           -- the current startcode
     , alex_ust  :: AlexUserState  -- AlexUserState will be defined in the user program
     }

   newtype Alex a = Alex { unAlex :: AlexState
                                  -> Either String (AlexState, a) }

   runAlex :: ByteString -> Alex a -> Either String a

   -- token :: (AlexInput -> Int -> token) -> AlexAction token

All of the actions in your lexical specification have the same type as
in the ``monadUserState`` wrapper. It is only the types of the function
to run the monad and the type of the ``token`` function that change.

.. _types:

Type Signatures and Typeclasses
-------------------------------

The ``%token``, ``%typeclass``, and ``%action`` directives can be used
to cause Alex to emit additional type signatures in generated code. This
allows the use of typeclasses in generated lexers.

Generating Type Signatures with Wrappers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``%token`` directive can be used to specify the token type when any
kind of ``%wrapper`` directive has been given. Whenever ``%token`` is
used, the ``%typeclass`` directive can also be used to specify one or
more typeclass constraints. The following shows a simple lexer that
makes use of this to interpret the meaning of tokens using the ``Read``
typeclass:

.. code-block:: none

   %wrapper "basic"
   %token "Token s"
   %typeclass "Read s"

   tokens :-

   [a-zA-Z0-9]+ { mkToken }
   [ \t\r\n]+   ;

   {

   data Token s = Tok s

   mkToken :: Read s => String -> Token s
   mkToken = Tok . read

   lex :: Read s => String -> [Token s]
   lex = alexScanTokens

   }

Multiple typeclasses can be given by separating them with commas, for
example:

::

   %typeclass "Read s, Eq s"

Generating Type Signatures without Wrappers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type signatures can also be generated for lexers that do not use any
wrapper. Instead of the ``%token`` directive, the ``%action`` directive
is used to specify the type of a lexer action. The ``%typeclass``
directive can be used to specify the typeclass in the same way as with a
wrapper. The following example shows the use of typeclasses with a
"homegrown" monadic lexer:

.. code-block:: none

   {
   {-# LANGUAGE FlexibleContexts #-}

   module Lexer where

   import Control.Monad.State
   import qualified Data.Bits
   import Data.Word

   }

   %action "AlexInput -> Int -> m (Token s)"
   %typeclass "Read s, MonadState AlexState m"

   tokens :-

   [a-zA-Z0-9]+ { mkToken }
   [ \t\n\r]+   ;

   {

   alexEOF :: MonadState AlexState m => m (Token s)
   alexEOF = return EOF

   mkToken :: (Read s, MonadState AlexState m) =>
              AlexInput -> Int -> m (Token s)
   mkToken (_, _, _, s) len = return (Tok (read (take len s)))

   data Token s = Tok s | EOF

   lex :: (MonadState AlexState m, Read s) => String -> m (Token s)
   lex input = alexMonadScan

   -- "Boilerplate" code from monad wrapper has been omitted

   }

The ``%token`` directive may only be used with wrapper, and the
``%action`` can only be used when no wrapper is used.

The ``%typeclass`` directive cannot be given without the ``%token`` or
``%action`` directive.
