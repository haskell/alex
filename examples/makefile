# This makefile assumes that lx and happy are on the path and that ghc 0.26 is
# installed.

HC =	ghc

all: tiny xscan tkns state lit pp ctx examples.hs

tiny: Alex.o Tokens.o tiny.o 
	$(HC) -o tiny Alex.o Tokens.o tiny.o
	strip tiny

tkns: Alex.o Tokens.o tkns.o
	$(HC) -o tkns Alex.o Tokens.o tkns.o
	strip tkns

xscan: Alex.o xscan.o
	$(HC) -o xscan Alex.o xscan.o

xscan.o: xscan.hs
	ghc -H15M -c xscan.hs

state: Alex.o state.o
	$(HC) -o state Alex.o state.o
	strip state

lit: Alex.o lit.o
	$(HC) -o lit Alex.o lit.o
	strip lit

pp: Alex.o pp.o
	$(HC) -o pp Alex.o pp.o
	strip pp

ctx: Alex.o ctx.o
	$(HC) -o ctx Alex.o ctx.o
	strip ctx


%.hs: %.x
	alex $<

%.hs: %.lx
	alex $<

%.hs: %.y
	happy $<

%.o: %.hs
	$(HC) -c $<


clean:
	rm -f tiny tiny.hs tkns Tokens.hs xscan xscan.hs state state.hs lit lit.hs pp pp.hs ctx ctx.hs examples.hs *.o *.hi
