#!/bin/sh

cd haskell
mkdir -p ./local-haddock
rm -rf ./local-haddock/*
cabal build --enable-documentation --haddock-hyperlink-source
cp -r dist-newstyle/build/x86_64-linux/ghc-8.10.4/maesarat-0.1.0.0/doc/html/maesarat/* ./local-haddock
