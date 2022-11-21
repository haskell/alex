.. _installing:

Obtaining Alex
==============

.. highlight:: bash

If you just want to *use* Alex, you can build from a release.
This should work the same as any other Haskell package.

Alex itself and its examples are intended to work with GHC >= 7.0.

Haskell-specific way
--------------------

From `Hackage <https://hackage.haskell.org/package/alex>`__ via `Cabal Install <https://www.haskell.org/cabal/>`__::

   $ cabal install alex

From `Stackage <https://www.stackage.org/package/alex>`__ via `Stack <https://haskellstack.org>`__::

   $ stack install --resolver nightly alex

Moreover, recent versions of ``cabal`` will automatically install the required version of ``alex`` based on ``build-tools``/``build-tool-depends`` `declarations <http://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-build-tool-depends>`__.

Operating System way
--------------------

Because Alex is a dependency of GHC, it is often packaged by operating systems.
`Repology <https://repology.org>`__ aggregates this info across many distros and operating systems, and Alex is actually listed twice:

- https://repology.org/project/haskell:alex/versions
- https://repology.org/project/alex/versions

The table contains links to the individual OS packages, which should provide installation instructions.
