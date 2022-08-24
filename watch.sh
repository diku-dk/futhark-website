#!/bin/sh

set -e

nix-build
result/bin/futhark-website watch
