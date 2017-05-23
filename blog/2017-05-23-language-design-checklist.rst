---
title: Language Design Checklist for Futhark
author: Troels Henriksen
description: Filling out the Language Design Checklist form for Futhark.
---

`Inspired by Myrddin <http://myrlang.org/lang-checklist>`_, here is
the Language Design Checklist for Futhark::

  You appear to be advocating a new:
  [x] functional  [ ] imperative  [ ] object-oriented  [ ] procedural [ ] stack-based
  [ ] "multi-paradigm"  [ ] lazy  [x] eager  [x] statically-typed  [ ] dynamically-typed
  [x] pure  [ ] impure  [ ] non-hygienic  [ ] visual  [ ] beginner-friendly
  [ ] non-programmer-friendly  [ ] completely incomprehensible
  programming language.  Your language will not work.  Here is why it will not work.

  You appear to believe that:
  [ ] Syntax is what makes programming difficult
  [ ] Garbage collection is free                [x] Computers have infinite memory
  [ ] Nobody really needs:
      [x] concurrency  [x] a REPL  [x] debugger support  [x] IDE support  [x] I/O
      [ ] to interact with code not written in your language
  [ ] The entire world speaks 7-bit ASCII
  [ ] Scaling up to large software projects will be easy
  [x] Convincing programmers to adopt a new language will be easy
  [ ] Convincing programmers to adopt a language-specific IDE will be easy
  [ ] Programmers love writing lots of boilerplate
  [x] Specifying behaviors as "undefined" means that programmers won't rely on them
  [ ] "Spooky action at a distance" makes programming more fun

  Unfortunately, your language (has/lacks):
  [Y] comprehensible syntax  [N] semicolons  [N] significant whitespace  [N] macros
  [N] implicit type conversion  [Y] explicit casting  [Y] type inference
  [N] goto  [N] exceptions  [N] closures  [N] tail recursion  [N] coroutines
  [N] reflection  [N] subtyping  [N] multiple inheritance  [N] operator overloading
  [N] algebraic datatypes  [N] recursive types  [Y] polymorphic types
  [N] covariant array typing  [N] monads  [N] dependent types
  [Y] infix operators  [N] nested comments  [N] multi-line strings  [N] regexes (lib)
  [Y] call-by-value  [N] call-by-name  [N] call-by-reference  [N] call-cc

  The following philosophical objections apply:
  [ ] Programmers should not need to understand category theory to write "Hello, World!"
  [ ] Programmers should not develop RSI from writing "Hello, World!"
  [ ] The most significant program written in your language is its own compiler
  [x] The most significant program written in your language isn't even its own compiler
  [x] No language spec
  [ ] "The implementation is the spec"
  [ ] The implementation is closed-source  [ ] covered by patents  [ ] not owned by you
  [ ] Your type system is unsound  [ ] Your language cannot be unambiguously parsed
  [ ] a proof of same is attached
  [ ] invoking this proof crashes the compiler
  [ ] The name of your language makes it impossible to find on Google
  [ ] Interpreted languages will never be as fast as C
  [x] Compiled languages will never be "extensible"
  [ ] Writing a compiler that understands English is AI-complete
  [x] Your language relies on an optimization which has never been shown possible
  [ ] There are less than 100 programmers on Earth smart enough to use your language
  [ ] ____________________________ takes exponential time
  [ ] ____________________________ is known to be undecidable

  Your implementation has the following flaws:
  [x] CPUs do not work that way
  [ ] RAM does not work that way
  [ ] VMs do not work that way
  [x] Compilers do not work that way
  [ ] Compilers cannot work that way
  [ ] Shift-reduce conflicts in parsing seem to be resolved using rand()
  [ ] You require the compiler to be present at runtime
  [ ] You require the language runtime to be present at compile-time
  [x] Your compiler errors are completely inscrutable
  [ ] Dangerous behavior is only a warning
  [x] The compiler crashes if you look at it funny
  [ ] The VM crashes if you look at it funny
  [x] You don't seem to understand basic optimization techniques
  [ ] You don't seem to understand basic systems programming
  [ ] You don't seem to understand pointers
  [ ] You don't seem to understand functions

  Additionally, your marketing has the following problems:
  [ ] Unsupported claims of increased productivity
  [ ] Unsupported claims of greater "ease of use"
  [x] Obviously rigged benchmarks
  [ ] Graphics, simulation, or crypto benchmarks where your code just calls
      handwritten assembly through your FFI
  [ ] String-processing benchmarks where you just call PCRE
  [ ] Matrix-math benchmarks where you just call BLAS
  [x] Noone really believes that your language is faster than:
      [x] assembly  [x] C  [x] FORTRAN  [ ] Java  [ ] Ruby  [ ] Prolog
  [ ] Rejection of orthodox programming-language theory without justification
  [x] Rejection of orthodox systems programming without justification
  [x] Rejection of orthodox algorithmic theory without justification
  [ ] Rejection of basic computer science without justification

  Taking the wider ecosystem into account, I would like to note that:
  [x] Your complex sample code would be one line in: __APL__________________
  [ ] We already have an unsafe imperative language
  [ ] We already have a safe imperative OO language
  [x] We already have a safe statically-typed eager functional language
  [ ] You have reinvented Lisp but worse
  [ ] You have reinvented Javascript but worse
  [ ] You have reinvented Java but worse
  [ ] You have reinvented C++ but worse
  [ ] You have reinvented PHP but worse
  [ ] You have reinvented PHP better, but that's still no justification
  [ ] You have reinvented Brainfuck but non-ironically

  In conclusion, this is what I think of you:
  [x] You have some interesting ideas, but this won't fly.
  [ ] This is a bad language, and you should feel bad for inventing it.
  [ ] Programming in this language is an adequate punishment for inventing it.

Although at least the "relies on  an optimization which has never been
shown possible" should become decrasingly true over time!
