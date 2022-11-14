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

Alex might also be pre-packaged for your OS (unconfirmed as of 2022-04-06):

- Ubuntu users: packages should be available from the universe repository.
- Debian GNU/Linux users: packages are available `here <http://packages.debian.org/alex>`__.
- FreeBSD/x86 users: Alex is in the ports tree, so either ``pkg_add -r hs-alex``,
  or install it from the ports tree in ``/usr/ports/devel/hs-alex``.
- OpenBSD users: Alex is in the -current ports tree, you can install it from ``/usr/ports/devel/alex``.
