HAPPY = happy 
HAPPY_OPTS = -agc

ALEX = alex
ALEX_OPTS = -g

sdist ::
	@if [ "`git status -s`" != '' ]; then \
		echo Tree is not clean; \
		exit 1; \
	fi
	$(HAPPY) $(HAPPY_OPTS) src/Parser.y -o src/Parser.hs
	$(ALEX) $(ALEX_OPTS) src/Scan.x -o src/Scan.hs
	mv src/Parser.y src/Parser.y.boot
	mv src/Scan.x src/Scan.x.boot
	cabal new-run gen-alex-sdist
	cabal sdist
	git checkout .
	git clean -f
