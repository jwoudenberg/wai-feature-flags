#!/usr/bin/env bash

set -euxo pipefail

name=$(grep name: < package.yaml | awk '{print $2}')
version=$(grep version: < package.yaml | awk '{print $2}')
bundle="$name-$version.tar.gz"

# Generate code
hpack
pushd frontend; elm make src/Main.elm --output index.html; popd

# check changelog contains an entry for this version
grep "^# $version$" < CHANGELOG.md

# check github release tag exists
git fetch --tags
git tag -l --points-at HEAD | grep "^$version$"

cabal sdist -o - > "$bundle"
cabal upload --publish "$bundle"
cabal upload -d --publish