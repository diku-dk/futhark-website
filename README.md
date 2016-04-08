The futhark Website
==

You need Hakyll installed to work on the site (`cabal install hakyll`).

To build: `runhaskell site.hs build`.

You can preview with: `runhaskell site.hs watch`.

When you are ready to deploy to
[futhark-lang.org](http://futhark-lang.org), run `runhaskell site.hs
deploy`.  This requires `rsync` installed locally and your public SSH
key added to the remote server.
