#!/bin/sh

set -e

runhaskell site.hs clean
runhaskell site.hs build
runhaskell site.hs deploy
git push
