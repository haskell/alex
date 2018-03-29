# Alex: A Lexical Analyser Generator

[![Build Status](https://secure.travis-ci.org/simonmar/alex.png?branch=master)](http://travis-ci.org/simonmar/alex)

Alex is a Lex-like tool for generating Haskell scanners.  For complete
documentation, see the doc directory.

- <https://www.haskell.org/alex/>

- <https://hackage.haskell.org/package/alex>

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

## Build Instructions

If you just want to *use* Alex, you can download or install (via
`cabal install alex`) an
[Alex release from Hackage](https://hackage.haskell.org/package/alex); also note that
distributions such as the
[Haskell Platform](https://www.haskell.org/platform/) and other package
manager-based distributions provide packages for Alex. Moreover,
recent versions of `cabal` will automatically install the required
version of `alex` based on
[`build-tools`/`build-tool-depends` declarations](http://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-build-tool-depends).

Read on if you want to build Alex directly from Git.

Alex is built using GHC & Cabal; so first install
[GHC](https://www.haskell.org/ghc) and
[`cabal-install-2.0`](https://www.haskell.org/cabal) (or later).

Since Alex itself is implemented in terms of an Alex scanner,
bootstrapping Alex is a bit tricky:

You need to have the build-tools `alex` and `happy` manually
installed; either via your system package manager distribution, the
Haskell Platform, or e.g. via (run this outside the Git repository!):

    $ cabal install alex happy

which installs them into `${HOME}/.cabal/bin` by default (make sure
they're in your `$PATH` for the next steps!).

### Variant A

First you need to generate the pre-processed templates via

    $ cabal new-run gen-alex-sdist

(otherwise `cabal install` will complain about
"`data/AlexTemplate: copyFile: does not exist (No such file or directory)`")

And then you can install `alex` simply by invoking

    $ cabal install
    
from inside the Git folder.

### Variant B

Alternatively, you can use the `Makefile` which automates the steps of
producing a self-contained pre-bootstrapped source distribution with
pre-generated lexer/scanners (and which also performs the `cabal
new-run gen-alex-sdist` pre-preprocessing step):

    $ make sdist
    $ cabal install dist/alex-*.tar.gz

For convenience, there's also a `make sdist-test` target which builds the
source source tarball and runs the test-suite from within the source dist.

## Contributing & Reporting Issues

Please report any bugs or comments at  https://github.com/simonmar/alex/issues

Share and enjoy,

Chris Dornan:  cdornan@arm.com

Isaac Jones:   ijones@syntaxpolice.org

Simon Marlow:  simonmar@microsoft.com
