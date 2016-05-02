The Futhark Website
==

The website is built with Hakyll, which is a Haskell library for
generating static sites.  The actual content is written in the
ReStructured Text format.

We use the [Haskell Tool Stack](http://docs.haskellstack.org/) for
managing compilation.  To build:

    stack exec futhark-website build

You can preview with:

    stack exec futhark-website watch

When you are ready to deploy to
[futhark-lang.org](http://futhark-lang.org), run:

    stack exec futhark-website deploy

This requires `rsync` installed locally and your public SSH key added
to the remote server.
