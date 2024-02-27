## Changes in 3.5.1.0

* Drop generating output for GHC < 6.4.
* Use qualified imports in generated code (except for `Prelude`)
  ([Issue #258](https://github.com/haskell/alex/issues/258)).
* Suppress warnings `tabs` and `unused-imports` for generated code
  ([Issue #255](https://github.com/haskell/alex/issues/255)).
* Tested with GHC 8.0 - 9.8.2.

_Andreas Abel, 2024-02-29_

## Changes in 3.5.0.0

 * Add option `--numeric-version`.
 * Remove deprecated `-v` as alias for `--version`.
 * Add `-v` as placeholder for a future `--verbose` option.
 * Make `alex{G,S}etUserState` available with the `monadUserState-bytestring` wrapper
   ([Issue #220](https://github.com/haskell/alex/issues/220)).
 * Debugging lexer: print character in addition to its ASCII code
   ([PR #252](https://github.com/haskell/alex/pull/252)).
 * Tested with GHC 8.0 - 9.8.1.

_Andreas Abel, 2023-12-30_

## Changes in 3.4.0.1

 * Address new `x-partial` warning of GHC 9.8.
 * Alex 3.4.0.1 needs GHC 8.0 or higher to build.
   The code it generates is the same as 3.4.0.0, so it will likely work for older GHCs.
 * Tested with GHC 8.0 - 9.8.1.

_Andreas Abel, 2023-10-29_

## Changes in 3.4.0.0

 * New wrappers to lex strict `Text`:
   `strict-text`, `posn-strict-text`, `monad-strict-text` and `monadUserState-strict-text`
   (PR [#240](https://github.com/haskell/alex/pull/240)).
   These complement the existing wrappers for `String` and `ByteString`.
 * Tested with GHC 7.0 - 9.6.2.

_Andreas Abel, 2023-06-20_

## Changes in 3.3.0.0

 * Add an `Ord` instance to `AlexPosn` (Issue [#233](https://github.com/haskell/alex/issues/233)).
   This breaks developments that define their own (orphan) `instance Ord AlexPosn`.
   If this is the derived stock instance, the fix is to delete the orphan instance and require
   `build-tool-depends: alex:alex >= 3.3.0.0`.
 * Switch to Haskell PVP versioning with four digits.
 * Tested with GHC 7.0 - 9.6.1.

_Andreas Abel, 2023-05-25_

## Change in 3.2.7.4

 * The user-supplied "epilogue" Haskell code is now put _last_ in the generated file.
   This enables use of Template Haskell in the epilogue.
   (Issue [#125](https://github.com/haskell/alex/issues/125).)
 * Tested with GHC 7.0 - 9.6.1.

_Andreas Abel, 2023-05-02_

## Change in 3.2.7.3

 * Amend last change (3.2.7.2)
   so that Alex-generated code does not need `LANGUAGE PatternGuards`.
 * Tested with GHC 7.0 - 9.6.1.

_Andreas Abel, 2023-04-14_

## Change in 3.2.7.2

 * Fix bug with out-of-bound access to `alex_check` array.
   (Surfaced with GHC's JS backend, fixed by Sylvain Henry in
    PR [#223](https://github.com/haskell/alex/pull/223).)
 * Tested with GHC 7.0 - 9.6.1.

_Andreas Abel, 2023-04-03_

## Change in 3.2.7.1

 * Fix bug with repeated numeral characters *outside* of `r{n,m}`
   repetitions. This was a regression introduced in 3.2.7.

_John Ericson, 2022-01-23_

## Changes in 3.2.7

 * Allow arbitrary repetitions in regexps.
   Previously, the `r{n,m}` and related forms were restricted to single
   digit numbers `n` and `m`.

 * DFA minimization used to crash on tokens of the form `c*` which
   produce automata with only accepting states.  Considering the empty
   set of non-accepting states as an equivalence class caused
   minimization to crash with exception.

 * The `small_base` flag is removed.  Extremely old GHCs will no longer
   build.

 * A number of bug fixes and clearer diagnostics.

_John Ericson, 2022-01-20_

## Changes in 3.2.6:

 * Support for the GHC 9.2.

   The array access primops now use the fixed-sized numeric types
   corresponding to the width of the data accessed. Additionally, the
   primops to convert to and from fixed-sized numeric types have been
   given new names.

   9.2 isn't cut yet, so these changes are somewhat speculative.
   Unfortunately, GHC must used a released version of Alex (and Happy)
   at all times until further changes have been made, so we must make
   the release to actually implement these changes in GHC.

   If the final GHC 9.2 ends up being different, this release will be
   marked broken to make it less likely people use it by accident.

_John Ericson, 2020-12-15_

## Changes in 3.2.5:

 * Build fixes for GHC 8.8.x

_Simon Marlow, 2019-11-04_

## Changes in 3.2.4:

 * Remove dependency on `QuickCheck`
 * Change the way that bootstrapping is done: see README.md for build
   instructions

_Simon Marlow, 2018-03-29_

## Changes in 3.2.3:

 * fix issue when using `cpphs` (#116)

_Simon Marlow, 2017-09-08_

## Changes in 3.2.2:

 * Manage line length in generated files [GH-84]
 * Fix issue when identifier with multiple single quotes, e.g. `foo''` was used
 * Allow omitting spaces around `=` in macro definitions
 * Include pre-generated `Parser.hs` and `Scan.hs` in the Hackage upload, to
   make bootstrapping easier.

_Simon Marlow, 2017-09-02_

## Changes in 3.2.1:

 * Fix build problem with GHC; add new test `tokens_scan_user.x`

_Simon Marlow, 2016-10-18_

## Changes in 3.2.0:

 * Allow the token type and productions to be overloaded, and add new
   directives: `%token`, `%typeclass`, `%action`.  See "Type Signatures and
   Typeclasses" in the manual.
 * Some small space leak fixes

_Simon Marlow, 2016-10-08_

##  Changes in 3.1.7:

 * Add support for `%encoding` directive
   (allows to control `--latin1` from inside Alex scripts)
 * Make code forward-compatible with in-progress proposals
 * Suppress more warnings

_Simon Marlow, 2016-01-08_

##  Changes in 3.1.6:

 * `sdist` for 3.1.5 was mis-generated, causing it to ask for Happy
    when building.

_Simon Marlow, 2015-11-30_

## Changes in 3.1.5:

 * Generate less warning-laden code, and suppress other warnings.
 * Bug fixes.

_Simon Marlow, 2015-11-25_

##  Changes in 3.1.4:

 * Add `Applicative`/`Functor` instances for GHC 7.10

_Simon Marlow, 2015-01-06_

##  Changes in 3.1.3:

 * Fix for `clang` (XCode 5)

_Simon Marlow, 2013-11-28_

##  Changes in 3.1.2:

 * Add missing file to `extra-source-files`

 _Simon Marlow, 2013-11-11_

##  Changes in 3.1.1:

 * Bug fixes (#24, #30, #31, #32)

_Simon Marlow, 2013-11-11_

##  Changes in 3.1.0:

 * necessary changes to work with GHC 7.8.1

_Simon Marlow, 2013-09-16_

##  Changes in 3.0 (since 2.3.5)

 * Unicode support (contributed mostly by Jean-Philippe Bernardy,
   with help from Alan Zimmerman).

   * An Alex lexer now takes a UTF-8 encoded byte sequence as input.
     If you are using the
     "basic" wrapper or one of the other wrappers that takes a
     Haskell String as input, the string is automatically encoded
     into UTF-8 by Alex. If your input is a `ByteString`, you are
     responsible for ensuring that the input is UTF-8 encoded.

   * Alex source files are assumed to be in UTF-8, like Haskell
     source files. The lexer specification can use Unicode
     characters and ranges.

   * `alexGetChar` is renamed to `alexGetByte` in the generated code.

   * There is a new option, `--latin1`, that restores the old 8-bit
     behaviour.

 * Alex now does DFA minimization, which helps to reduce the size
   of the generated tables, especially for lexers that use Unicode.

## Release Notes for version 2.2

-   `Cabal-1.2` is now required.

-   `ByteString` wrappers: use Alex to lex ByteStrings directly.

## Release Notes for version 2.1.0

-   Switch to a Cabal build system: you need a recent version of Cabal
    (1.1.6 or later). If you have GHC 6.4.2, then you need to upgrade
    Cabal before building Alex. GHC 6.6 is fine.

-   Slight change in the error semantics: the input returned on error is
    before the erroneous character was read, not after. This helps to
    give better error messages.

## Release Notes for version 2.0

Alex has changed a *lot* between versions 1.x and 2.0. The following is
supposed to be an exhaustive list of the changes:

### Syntax changes

-   Code blocks are now surrounded by `{...}` rather than `%{...%}`.

-   Character-set macros now begin with ‘`$`’ instead of ‘`^`’ and have
    multi-character names.

-   Regular expression macros now begin with ‘`@`’ instead of ‘`%`’ and
    have multi-character names.

-   Macro definitions are no longer surrounded by `{ ... }`.

-   Rules are now of the form

        <c1,c2,...>  regex   { code }

    where `c1`, `c2` are startcodes, and `code` is an arbitrary Haskell
    expression.

-   Regular expression syntax changes:

    -   `()` is the empty regular expression (used to be ‘`$`’)

    -   set complement can now be expressed as `[^sets]` (for similarity
        with lex regular expressions).

    -   The `'abc'` form is no longer available, use `[abc]` instead.

    -   ‘`^`’ and ‘`$`’ have the usual meanings: ‘`^`’ matches just
        after a ‘`\n`’, and ‘`$`’ matches just before a ‘`\n`’.

    -   ‘`\n`’ is now the escape character, not ‘`^`’.

    -   The form `"..."` means the same as the sequence of characters
        inside the quotes, the difference being that special characters
        do not need to be escaped inside `"..."`.

-   Rules can have arbitrary predicates attached to them. This subsumes
    the previous left-context and right-context facilities (although
    these are still allowed as syntactic sugar).

### Changes in the form of an Alex file

-   Each file can now only define a single grammar. This change was made
    to simplify code generation. Multiple grammars can be simulated
    using startcodes, or split into separate modules.

-   The API has been simplified, and at the same time made more
    flexible.

-   You no longer need to import the `Alex` module.

### Usage changes

The command-line syntax is quite different.

### Implementation changes

-   A more efficient table representation, coupled with standard
    table-compression techniques, are used to keep the size of the
    generated code down.

-   When compiling a grammar with GHC, the `-g` switch causes an even
    faster and smaller grammar to be generated.

-   Startcodes are implemented in a different way: each state
    corresponds to a different initial state in the DFA, so the scanner
    doesn't have to check the startcode when it gets to an accept state.
    This results in a larger, but quicker, scanner.
