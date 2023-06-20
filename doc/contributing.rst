.. _contributing:

Contributing to Alex
====================

.. highlight:: bash

Source Code Repository
----------------------

Alex is hosted on `GitHub <https://github.com/haskell/alex>`__.
As previously discussed in :ref:`bug-reports`, we use the built-in `GitHub issue tracker <https://github.com/haskell/alex/issues>`__ for Alex.
We also use `GitHub pull requests <https://github.com/haskell/alex/pulls>`__ for managing changes;
feel free to submit them!

Repo Layout
-----------

- ``src``: The source code for Alex itself
- ``doc``: The documentation
- ``examples``: Various examples of using Alex

Contributor Build Instructions
------------------------------

Alex is built using `GHC <https://www.haskell.org/ghc>`__ and
`Cabal Install <https://www.haskell.org/cabal>`__ (version 2.0 or later).
Make sure they are already installed first.

Since Alex itself is implemented in terms of an Alex scanner,
bootstrapping Alex is a bit tricky:

You need to have the build-tools ``alex`` and ``happy`` manually installed;
either via your system package manager distribution, the Haskell Platform, or e.g. via (run this outside the Git repository!)::

   $ cabal install alex happy

which installs them into ``${HOME}/.cabal/bin`` by default.
(make sure they are in your ``$PATH`` for the next steps!)

Variant A
~~~~~~~~~

You can install ``alex`` simply by invoking::

   $ cabal install

from inside the Git folder.

Variant B
~~~~~~~~~

Alternatively, you can use the ``Makefile`` which automates the steps of producing a self-contained pre-bootstrapped source distribution with pre-generated lexer/scanners::

   $ make sdist
   $ cabal install dist/alex-*.tar.gz

For convenience, there is also a ``make sdist-test`` target which builds the source source tarball and runs the test-suite from within the source dist.
