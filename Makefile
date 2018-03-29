CABAL = cabal

HAPPY = happy 
HAPPY_OPTS = -agc

ALEX = alex
ALEX_OPTS = -g
ALEX_VER = `awk '/^version:/ { print $$2 }' alex.cabal`

sdist ::
	@case "`$(CABAL) --numeric-version`" in \
		2.[2-9].* | [3-9].* ) ;; \
		* ) echo "Error: needs cabal 2.2.0.0 or later (but got : `$(CABAL) --numeric-version`)" ; exit 1 ;; \
	esac
	@if [ "`git status -s`" != '' ]; then \
		echo "Error: Tree is not clean"; \
		exit 1; \
	fi
	rm -rf dist/
	$(HAPPY) $(HAPPY_OPTS) src/Parser.y -o src/Parser.hs
	$(ALEX) $(ALEX_OPTS) src/Scan.x -o src/Scan.hs
	mv src/Parser.y src/Parser.y.boot
	mv src/Scan.x src/Scan.x.boot
	$(CABAL) new-run gen-alex-sdist
	$(CABAL) sdist
	@if [ ! -f "dist/alex-$(ALEX_VER).tar.gz" ]; then \
		echo "Error: source tarball not found: dist/alex-$(ALEX_VER).tar.gz"; \
		exit 1; \
	fi
	git checkout .
	git clean -f

sdist-test :: sdist sdist-test-only
	@rm -rf "dist/alex-$(ALEX_VER)/"

sdist-test-only ::
	@if [ ! -f "dist/alex-$(ALEX_VER).tar.gz" ]; then \
		echo "Error: source tarball not found: dist/alex-$(ALEX_VER).tar.gz"; \
		exit 1; \
	fi
	rm -rf "dist/alex-$(ALEX_VER)/"
	tar -xf "dist/alex-$(ALEX_VER).tar.gz" -C dist/
	echo "packages: ." > "dist/alex-$(ALEX_VER)/cabal.project"
	cd "dist/alex-$(ALEX_VER)/" && cabal new-test --enable-tests all
	@echo ""
	@echo "Success! dist/alex-$(ALEX_VER).tar.gz is ready for distribution!"
	@echo ""
