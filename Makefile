.PHONY: all clean dist docs

nixsh = nix-shell --pure --command


all: dist/setup-config Setup default.nix shell.nix
	$(nixsh) "./Setup build"

clean:
	rm -rf dist
	rm -f Setup Setup.hi Setup.o shell.nix

dist: dist/setup-config Setup default.nix shell.nix
	$(nixsh) "./Setup sdist"


Setup: Setup.lhs shell.nix
	$(nixsh) "ghc -O -o $@ $<"
	@touch Setup

default.nix: $(wildcard *.cabal)
	cabal2nix ./. > $@

shell.nix: $(wildcard *.cabal)
	cabal2nix -fexamples --shell ./. > $@

dist/setup-config: $(wildcard *.cabal) Setup shell.nix
	$(nixsh) "./Setup configure -fexamples"
