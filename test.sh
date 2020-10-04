#!/usr/bin/env bash

set -euo pipefail

# Check all Haskell files are formatted using Ormolu
LC_ALL=C.UTF-8 ormolu -m inplace src/**/*.hs
# Check all Elm files are formatted using elm-format
elm-format --validate ./frontend/src/**.elm
if git status --porcelain | grep . ; then
  echo "Not all files were formatted."
  echo "To fix this error, run ./test.sh and commit the result."
  git diff --no-color
  exit 1
fi

# Compilation succeeds
pushd frontend; elm-test; popd
hpack
cabal build
