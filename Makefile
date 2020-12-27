CABAL = cabal

HAPPY = happy 
HAPPY_OPTS = -agc

ALEX = alex
ALEX_OPTS = -g
ALEX_VER = `awk '/^version:/ { print $$2 }' alex.cabal`

SDIST_DIR=dist-newstyle/sdist

sdist ::
	@case "`$(CABAL) --numeric-version`" in \
		2.[2-9].* | [3-9].* ) ;; \
		* ) echo "Error: needs cabal 2.2.0.0 or later (but got : `$(CABAL) --numeric-version`)" ; exit 1 ;; \
	esac
	@if [ "`git status -s`" != '' ]; then \
		echo "Error: Tree is not clean"; \
		exit 1; \
	fi
	$(CABAL) v2-run gen-alex-sdist
	$(CABAL) v2-sdist
	@if [ ! -f "${SDIST_DIR}/alex-$(ALEX_VER).tar.gz" ]; then \
		echo "Error: source tarball not found: dist/alex-$(ALEX_VER).tar.gz"; \
		exit 1; \
	fi
	git checkout .
	git clean -f

sdist-test :: sdist sdist-test-only
	@rm -rf "${SDIST_DIR}/alex-${ALEX_VER}/"

sdist-test-only ::
	@if [ ! -f "${SDIST_DIR}/alex-$(ALEX_VER).tar.gz" ]; then \
		echo "Error: source tarball not found: ${SDIST_DIR}/alex-$(ALEX_VER).tar.gz"; \
		exit 1; \
	fi
	rm -rf "${SDIST_DIR}/alex-$(ALEX_VER)/"
	tar -xf "${SDIST_DIR}/alex-$(ALEX_VER).tar.gz" -C ${SDIST_DIR}/
	echo "packages: ." > "${SDIST_DIR}/alex-$(ALEX_VER)/cabal.project"
	echo "tests: True" >> "${SDIST_DIR}/alex-$(ALEX_VER)/cabal.project"
	cd "${SDIST_DIR}/alex-$(ALEX_VER)/" \
		&& cabal v2-build all --flag -bootstrap \
		&& cabal v2-install --flag -bootstrap --installdir="./bootstrap-root" \
		&& cabal v2-test all -j --flag -bootstrap \
		&& export PATH=./bootstrap-root:$$PATH \
		&& cabal v2-build all --flag +bootstrap \
		&& cabal v2-test all -j --flag +bootstrap
	@echo ""
	@echo "Success! ${SDIST_DIR}/alex-$(ALEX_VER).tar.gz is ready for distribution!"
	@echo ""
