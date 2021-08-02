#!/bin/sh

set -e

cabal exec futhark-website clean
cabal exec futhark-website build
cabal exec futhark-website deploy
git push
