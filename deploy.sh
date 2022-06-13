#!/bin/sh

set -e

nix-build
result/bin/futhark-website clean
result/bin/futhark-website build
result/bin/futhark-website deploy
git push
