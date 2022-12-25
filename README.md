# The Futhark Website

The website is built with Hakyll, which is a Haskell library for
generating static sites.  The actual content is written in the
ReStructured Text and Markdown formats.

## Building with [Nix](https://nixos.org/)

This is much faster than using Cabal.

Use `sh watch.sh` to run a local test of the site.

Use `sh deploy.sh` to push it to the web server.

## Building with Cabal

Install `futhark-website` package first:

    cabal install futhark-website.cabal

To build the website:

    cabal exec futhark-website build

You can preview with:

    cabal exec futhark-website watch

When you are ready to deploy to
[futhark-lang.org](http://futhark-lang.org), run:

    cabal exec futhark-website deploy

This requires `rsync` installed locally and your public SSH key added
to the remote server.  You can also just push to the Git repository,
where a GitHub Actions job will then take care of building and
deploying to the server.
