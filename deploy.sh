#!/bin/sh

set -e

cabal run futhark-website clean
cabal run futhark-website build
cabal run futhark-website deploy
git push
