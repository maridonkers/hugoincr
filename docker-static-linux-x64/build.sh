#!/bin/sh

cd ~/Development/Projects/HugoIncr/haskell/hugoincr
cabal build --enable-executable-static --with-compiler=ghc exe:hugoincr

cp ./dist-newstyle/build/x86_64-linux/ghc-9.0.2/hugoincr-0.1.2/x/hugoincr/build/hugoincr/hugoincr ./docker-static-linux-x64
strip ./docker-static-linux-x64/hugoincr
