#!/usr/bin/env bash
rm bin/ga-svm
ghc -o bin/ga-svm src/ga-svm.hs
#ghc --make -Wall src/ga-svm.hs
bin/ga-svm data/bcw-gasvm.model
