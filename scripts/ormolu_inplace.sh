#!/bin/sh

find ./haskell/src -name '*.hs' | xargs ormolu \
  --ghc-opt -XImportQualifiedPost \
  --ghc-opt -XTypeApplications \
  --ghc-opt -XPatternSynonyms \
  --mode=inplace

find ./haskell/app -name '*.hs' | xargs ormolu \
  --ghc-opt -XImportQualifiedPost \
  --ghc-opt -XTypeApplications \
  --ghc-opt -XPatternSynonyms \
  --mode=inplace
