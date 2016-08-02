# Alex: A Lexical Analyser Generator

[![Build Status](https://secure.travis-ci.org/simonmar/alex.png?branch=master)](http://travis-ci.org/simonmar/alex)

Alex is a Lex-like tool for generating Haskell scanners.  For complete
documentation, see the doc directory.

   http://www.haskell.org/alex/

   http://hackage.haskell.org/package/alex

Alex is built using Cabal.  First install GHC and cabal-install, then:

    $ cabal configure
    $ cabal build
    $ cabal install

Alex version 2.0 has changed fairly considerably since version 1.x,
and the syntax is almost completely different.  For a detailed list of
changes, see the release notes in the documentation.

Alex is now covered by a BSD-Style licence; see the licence file in
the 'doc' directory for details.

The sources are in the 'src' directory and the documentation in the 'doc'
directory; various  examples are in the 'examples' subdirectory.

The source code in the 'src' and 'examples' directories is intended
for a Haskell 98 compiler with hierarchical modules.  It should work
with GHC >= 5.04.

Please report any bugs or comments at  https://github.com/simonmar/alex/issues

Share and enjoy,

Chris Dornan:  cdornan@arm.com

Isaac Jones:   ijones@syntaxpolice.org

Simon Marlow:  simonmar@microsoft.com
