# Alex: A Lexical Analyser Generator

[![Haskell-CI](https://github.com/haskell/alex/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/haskell/alex/actions/workflows/haskell-ci.yml)

Alex is a Lex-like tool for generating Haskell scanners.  For complete
documentation, see the doc directory.

- <https://www.haskell.org/alex/>

- <https://hackage.haskell.org/package/alex>

For information on copying and distributing this program, see the file
LICENSE in this directory.

The sources are in the `src` directory and the documentation in the `doc`
directory; various  examples are in the `examples` subdirectory.

The source code in the `src` and `examples` directories is intended to work
with GHC >= 7.0.

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
they are in your `$PATH` for the next steps!).

### Variant A

You can install `alex` simply by invoking

    $ cabal install

from inside the Git folder.

### Variant B

Alternatively, you can use the `Makefile` which automates the steps of
producing a self-contained pre-bootstrapped source distribution with
pre-generated lexer/scanners:

    $ make sdist
    $ cabal install dist/alex-*.tar.gz

For convenience, there is also a `make sdist-test` target which builds the
source source tarball and runs the test-suite from within the source dist.

## Contributing & Reporting Issues

Please report any bugs or comments at  https://github.com/simonmar/alex/issues

Share and enjoy,

Chris Dornan:  cdornan@arm.com

Isaac Jones:   ijones@syntaxpolice.org

Simon Marlow:  simonmar@microsoft.com

and [recent contributors](https://github.com/simonmar/alex/graphs/contributors).

## Current Maintainers

- John Ericson (@Ericson2314)

- Simon Marlow (@simonmar)
