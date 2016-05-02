#!/bin/sh

set -e

stack exec futhark-website clean
stack exec futhark-website build
stack exec futhark-website deploy
git push
