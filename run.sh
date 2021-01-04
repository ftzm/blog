#!/usr/bin/env bash

# Rebuild on template/content change
find site/ | entr -p sh -c 'cabal run' &

# Rebuild on generator change
find app/ | entr -p sh -c 'rm -r .shake; rm -r docs; cabal run' &

# Clean up the terminal on exit
trap "reset" EXIT

# Serve static files
serve docs
