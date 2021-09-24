---
title: The Futhark Error Index
author: Troels Henriksen
description: See if your favourite is here!
---

Error messages are the main user interface of a compiler.  Many
languages are [famous for the incomprehensibility of their error
messages](https://tgceec.tumblr.com/), and fewer have become famous
for their quality.  Rust and [Elm](https://elm-lang.org/) are examples
from the latter category.  Now, while I would certainly like Futhark
to join them, generating good error messages is hard.  And the more
complicated ("powerful") you make your type system, the harder it
becomes to explain to the programmer why their program is wrong, and
how they should fix it.  It becomes tempting to turn every error
message into a language tutorial that explains every possibly relevant
detail, but this is not really practical.  I also have a personal
preference for somewhat austere errors that don't contain significant
bottled text without pertinent information.

Rust has a nice compromise.  Each class of errors is numbered, and for
each error, the [Rust Compiler Error
Index](https://doc.rust-lang.org/error-index.html) contains an example
program that causes the error, an explanation of what is wrong, and a
suggested remedy.  The compiler error itself can then contain merely a
link to the appropriate spot in the index, which helps the novice, but
also does not irritate the experienced programmer by drowning out
other information.  Motivated by [a bug report about a particularly
confusing type error](https://github.com/diku-dk/futhark/issues/1381),
I set about constructing something similar for Futhark.

I have previously written about [how Futhark is
developed](2018-06-18-designing-a-programming-language-for-the-desert.html) -
namely that we need to carefully manage our limited time, and
especially be careful not to increase our maintenance load.  In
particular, it is not realistic to maintain a stable numbering of all
compiler errors the way Rust does.  I just want to easily explain the
errors that I suspect will confuse programmers, and easily add new
explanations when new programmers report new sources of confusion.  So
for a start, the [Futhark Compiler Error
Index](https://futhark.readthedocs.io/en/latest/error-index.html)
contains mostly errors related to uniqueness and size types.  These
are the novel parts of Futhark's type system, and thus also the parts
most likely to confuse programmers.  Each error is identified by a
distinct HTML `id`, which allows error messages to link directly to
the explanation, e.g:

```
Error at test.fut:2:3-3:6:
Would consume "a", which is not consumable.

For more information, see:
  https://futhark.readthedocs.io/en/latest/error-index.html#not-consumable
```

Note the `latest` part of the URL.  A released version of Futhark will
replace that with the relevant version number, which prevents the link
from rotting, even though we may change the error categorisation in
later versions of the compiler (as long as Read The Docs stays
online).

The implementation in the type checker is quite simple.  This is the
function that generates the error above:

```Haskell
notConsumable :: MonadTypeChecker m => SrcLoc -> Doc -> m b
notConsumable loc v =
  typeError loc mempty . withIndexLink "not-consumable" $
    "Would consume" <+> v <> ", which is not consumable."
```

So it's very easy to add an explanation for an error.  Just use
`withIndexLink` with a distinct identifier, and add an explanation to
the appropriate place in the documentation.  There is not yet any
automatic testing that the identifier matches anything in the manual,
but I don't expect that would be difficult to add.

In conclusion, this solution has essentially zero maintenance
overhead, is stable across versions (thanks to Read The Docs, which
hosts copies for all releases), and makes it easy to gradually grow an
increasingly complete and (one hopes) understandable explanation of
the various ways your code disappoints the Futhark compiler.
