# Always run `make shell` first to enter a Nix shell. If you start
# Emacs from within a Nix shell then its Haskell support works with
# the project.
#

PROJECT="offlineimapgenerator"
EXECUTABLE="exe:Offlineimapgenerator"

all: help

# Always use `make shell` first, before other commands.
shell:
	LC_ALL=C.UTF-8 nix-shell

edit:
	emacs &

build:
	hpack
	cabal build

rebuild:
	cabal clean
	rm -f $(PROJECT).cabal
	hpack
	cabal build

update:
	cabal update

clean:
	cabal clean

run:
	cabal run . --

run-verbose:
	cabal run . -- -v

repl:
	cabal repl

ls:
	@cabal list-bin ${EXECUTABLE}

lint:
	ag --haskell -l | xargs hlint -v

formatter:
	ag --haskell -l | xargs stylish-haskell -i

help:
	@grep '^[^ 	#:]\+:' Makefile | sed -e 's/:[^:]*//g'
