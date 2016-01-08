##  Changes in 3.1.7:

 * Add support for `%encoding` directive
   (allows to control `--latin1` from inside Alex scripts)
 * Make code forward-compatible with in-progress proposals
 * Suppress more warnings

##  Changes in 3.1.6:

 * `sdist` for 3.1.5 was mis-generated, causing it to ask for Happy
    when building.

## Changes in 3.1.5:

 * Generate less warning-laden code, and suppress other warnings.
 * Bug fixes.

##  Changes in 3.1.4:

 * Add Applicative/Functor instances for GHC 7.10

##  Changes in 3.1.3:

 * Fix for clang (XCode 5)

##  Changes in 3.1.2:

 * Add missing file to extra-source-files

##  Changes in 3.1.1:

 * Bug fixes (#24, #30, #31, #32)

##  Changes in 3.1.0:

 * necessary changes to work with GHC 7.8.1

##  Changes in 3.0 (since 2.3.5)

 * Unicode support (contributed mostly by Jean-Philippe Bernardy,
   with help from Alan Zimmerman).

   * An Alex lexer now takes a UTF-8 encoded byte sequence as input
     (see Section 5.1, “Unicode and UTF-8”. If you are using the
     "basic" wrapper or one of the other wrappers that takes a
     Haskell String as input, the string is automatically encoded
     into UTF-8 by Alex. If your input is a ByteString, you are
     responsible for ensuring that the input is UTF-8 encoded. The
     old 8-bit behaviour is still available via the --latin1
     option.

   * Alex source files are assumed to be in UTF-8, like Haskell
     source files. The lexer specification can use Unicode
     characters and ranges.

   * `alexGetChar` is renamed to `alexGetByte` in the generated code.

   * There is a new option, `--latin1`, that restores the old
     behaviour.

 * Alex now does DFA minimization, which helps to reduce the size
   of the generated tables, especially for lexers that use Unicode.
