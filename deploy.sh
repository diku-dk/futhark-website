#!/bin/sh

set -e

runhaskell site.hs build
runhaskell site.hs deploy
git push
