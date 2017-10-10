---
title: Block Comments are a Bad Idea
author: Troels Henriksen
description: Futhark does not support block comments, and this post elaborates why.
---

Many programming languages support two notations for comments.  One is
*line comments*, which makes the parser disregard the remainder of the
line.  In C and related languages, these are written ``//`` (this
syntax is actually from C++, which took it from CPL, but that's
another story).  The other notation for comments is *block comments*,
in which the comment is enclosed by special separators.  In C, these
separators are ``/*`` and ``*/``.  In this post I will argue that
block comments are unnecessary, and in fact near-impossible to design
and implement correctly (for my own pedantic notion of correctness),
and so should be left out of future programming languages.  The title
and tone of this article is intentionally provocative: it is clear
that, in practice, a plethora of languages employ block comments with
no great trouble.  I will show that there are cases where these
designs fall flat, but I cannot in good faith claim that they occur
all that often in practice.  The intended audience of this text is
language designers as pedantic as me, who enjoy fretting over the
tiniest of inconsistencies.

Why some languages have block comments
--------------------------------------

Block comments serve two main purposes:

  **Writing long comments**: Significant bodies of text, maybe
  including ASCII art diagrams, where it is perceived that line
  comments would be impractical

  **Commenting out code**: When debugging, it is sometimes convenient
  to comment out large swathes of code.  Placing a begin- and
  end-marker is more convenient than manually prefixing each line with
  a line comment marker.

Unfortunately, it appears that no single block comment notation can
support both of these use cases without admitting unfortunate edge
cases.

When writing long comments, we want a great degree of freedom in the
contents of the comment.  We are likely willing to accept the
restriction that the comment body cannot contain the end-comment
marker, but that's it.  C-style block comments handle this case well.

When commenting out code, we want the property that for any given
syntactically valid piece of code, we can enclose it in block comment
markers, without modifying the contained code, and obtain a valid
comment that encloses the full piece of code.  Interestingly, C block
comments do not have this property.  The problem occurs when we
comment out code that already contains a block comment::

  /* set y */
  int y = x + 2;

Enclosing this in block comments produces::

  /*
  /* set y */
  int y = x + 2;
  */

Unfortunately, under C syntax rules, the block comment always ends at
the *first* seen ``*/``, which in this case occurs just before
``int``, and was part of the code we wanted to comment out in the
first place!  We ``*/`` we inserted is no longer part of a comment,
which will likely cause a parse error.

Being Smart about Block Comments
--------------------------------

Some languages attempt to address this by permitting nested block
comments, where the comment does not end until all begin-markers have
been matched by a corresponding end-marker.  Haskell is one of these
languages, and delineates block comments with ``{-`` and ``-}``.
Returning to our previous example, the following Haskell declaration::

  {- set y - }
  y = x + 2

Can be enclosed in a block comment as follows::

  {-
  {- set y - }
  y = x + 2
  -}

This will work as expected; commenting out the entire declaration.
However, this notation breaks down in more complicated/contrived
cases, as the compiler does not use a full lexer to scan the contents
of the comment.  This means that character sequences that were not
interpreted as comment delimiters in the original code may now be.
The primary cause of such trouble is string literals.  Consider::

  s = "-}"

This is perfectly cromulent Haskell.  But consider what happens as we
enclose it in block comments::

  {-
  s = "-}"
  -}

Oops - that comment terminates earlier than expected, at the end
marker previously enclosed in a string literal, leading to a syntax
error.  A similar error case occurs for comment openers::

  {-
  s = "{-"
  -}

Due to the support for nested comments, this comment now extends far
past what we intended for it.  While the result is likely still a
syntax error, the error message will probably mention either the
beginning of the comment or its eventual end (probably the end of the
file), neither of which is the true source of the problem.

One may be tempted to solve this issue by applying a lexer to the
comment body, and disregard those comment delimiters that occur inside
string literals.  However, this conflicts with the other use for block
comments, namely writing long natural-language texts.  I think most
programmers would quickly grow frustrated if even their *comments* had
to be valid according to the lexical rules of the programming
language.  We use comments to get away from all that!

One solution may be to employ an *error-tolerant lexer* that permits
arbitrary text, but still tries to detect string literals.  However,
such a best would likely be too unpredictable and complicated in
practice.  Consider::

  Just a quote: "                 Now a string: "-}"

Should this comment marker count or not?  This feels like the kind of
problem that is best solved by not introducing it in the first place,
which is exactly why Futhark does not support block comments.

Just Use Line Comments!
-----------------------

Block comment notation can certainly be convenient, but it has been my
experience that line comments can reach the same level of convenience
with just a little bit of editor support.  Specifically, any `decent
editor <https://www.gnu.org/software/emacs/>`_ contains commands for
adding or removing line comments for an entire block of code.
Furthermore, such a decent editor also contains facilities for
`wrapping long line-comments to 80 characters
<https://www.emacswiki.org/emacs/FillingComments>`_ (or whichever line
length you prefer), and automatically inserting or removing line
comment markers as necessary.  The only feature of block comments that
cannot be replicated directly is inserting a comment in the middle of
a line, but this is probably a niche usage.

If You Must Have Block Comments
-------------------------------

The problems described above can be addressed by supporting two
different forms of block comments: one for text and one for code,
where the latter must be lexically (or perhaps even syntactically)
valid.  C actually supports just this via the C preprocessor::

  #if 0
  ...
  #endif

For languages with preprocessors or real macro systems, this is a
decent solution.  In Common Lisp, we can use the ``#+nil`` reader
macro, which will cause the following S-expression to be disregarded.

For languages going this route, I suggest supporting standard block
comments only in their simplest form (as in C), rather than attempting
to handle nested comments the way Haskell does.  As we have seen,
there are still edge cases that it does not handle correctly.

Languages Without Block Comments
--------------------------------

Futhark is not the only language to eschew block comments.  Another is
`Erlang <http://erlang.org>`_, and they seem to manage alright.  Shell
script and Python also do not support block comments, although they do
support `heredocs <https://en.wikipedia.org/wiki/Here_document>`_ and
triple-quoted strings respectively, which can be employed to serve the
same purpose.  More examples exist - just take a look at Wikipedia's
`comparison of programming languages by comment syntax
<https://en.wikipedia.org/wiki/Comparison_of_programming_languages_(syntax)#Comment_comparison>`_
(because of course Wikipedia has such a list).
