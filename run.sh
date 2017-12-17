#!/usr/bin/env bash
rm ga-svm
ghc -o ga-svm src/ga-svm.hs
#ghc --make -Wall src/ga-svm.hs
./ga-svm data/bcw-gasvm.model
