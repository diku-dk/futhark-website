-- ---
-- title: Data parallel pretty-printing
-- description: A case study in how to pretty-print expression trees in Futhark.
-- ---

-- Apart from developing an excellent compiler with [nice
-- tools](2026-05-27-futhark-0.26.3-released.html) and [pleasant
-- properties](2026-05-22-benchmarking-a-real-futhark-application.html), we who
-- work on or near the Futhark project are also interested in [data parallel
-- algorithms](2026-05-06-count-trailing-zeros.html) for various problems, in
-- particular those that do not immediately fit the array programming paradigm. A
-- lot of these challenges are related to how to process trees or text, and in this
-- post I will go through a couple of solutions to a problem that involves both:
-- pretty-printing of abstract syntax trees. This is of course a problem that
-- arises often in compilers and similar tools, but as usual, the *real* point is
-- just to exercise techniques for non-obvious data parallel problems.
--
-- ## A representation of arithmetic trees
--
-- In this post we are going to focus on pretty-printing trees representing a
-- specific tiny language of arithmetic expressions, and we are going to fully
-- parenthesise every expression. I think the ideas generalise nicely, but that
-- is for the future. Specifically, we are going to be dealing with arithmetic
-- expressions that involve integers and operators for addition, subtraction,
-- multiplication, and division. In Haskell, we would define a data type for
-- such trees as follows:
--
-- ```Haskell
-- data Op = Add | Sub | Mul | Div
-- data Expr = Num Integer
--           | Op Expr Op Expr
-- ```
--
-- Pretty much any functional language would do it this way. However, in
-- Futhark, we encounter problems immediately: the language does not support
-- recursive data types, or even recursive functions at all. This means we need
-- to encode our trees as the only kind of arbitrary-sized aggregate data type
-- the language *does* support: arrays. There are many ways to do this, but we
-- will go for perhaps the most straightforward one: a tree is represented as an
-- array of nodes, and a node refers to a child by storing its index of the
-- child in the array:

type ptr = i64
type op = #add | #sub | #mul | #div
type expr = #num i32 | #op ptr op ptr

-- By convention, we say that the first node in an array of `expr`s is the root
-- of the tree. To be clear, I don't think this is a *nicer* representation than
-- a civilised recursive data type - it is very easy to make indexing errors -
-- but it does work. (There are ways to [tighten it up a little with fancy
-- type-level tricks](2021-10-16-explicit-existentials.html), but it will never
-- be as nice as in Haskell.) As an example, here is how we represent the AST
-- corresponding to the expression `(8+20)*42`:

def expression : []expr =
  [#op 3 #mul 4, #num 8, #num 20, #op 1 #add 2, #num 42]

-- ## Pretty-printing
--
-- Now let's say we want to write a function for pretty-printing this
-- expression. It goes without saying that massively parallel pretty-printing is
-- only *potentially* meaningful for huge trees, but I hope the reader will
-- forgive me for such a tiny example.

-- First, we have the usual problem that Futhark as a language really has no
-- good grasp of text. So let's start by stating that a character is a byte:

type char = u8

-- Let us now define some other functions we will need. First, a function that
-- given an operator returns its character representation:

def op_char (op: op) : char =
  match op
  case #add -> '+'
  case #sub -> '-'
  case #mul -> '*'
  case #div -> '/'

-- Similarly, we define two functions on integers: one that returns the number
-- of decimal digits in an integer (we assume non-negative numbers for
-- simplicity), and one that retrieves the *i*th digit of an integer (counting
-- from right to left):

def num_digits (x: i32) : i64 =
  i64.f64 (f64.log10 (f64.i32 x) + 1)

def ith_digit (x: i32) (i: i64) : char =
  '0' + u8.i32 (x / 10 ** i32.i64 i) % 10

-- Now we are ready to describe our first algorithm for pretty-printing these
-- arithmetic trees. A normal algorithm is based on recursively pretty-printing
-- the sub-trees of a node, then concatenating them (perhaps by using a clever
-- implementation of concatenation), but recursion is not an option in Futhark.
-- Instead, our solution is inspired by term rewriting: starting an array
-- containing just the root expression, we keep replacing expressions by their
-- "expansion" into a sequence of expressions and characters, which we call
-- *tokens* (not a great word; it is unrelated to lexical tokens). A token is
-- either a character or an expression:

type token = #char char | #expr expr

-- At any given step in our algorithm, we are dealing with an array of these
-- tokens - any `#char` element has reached its final form, but each `#expr` may
-- expand into any number of other tokens. Essentially we need to perform some
-- kind of `concat-map` over the array, but this is notoriously not something
-- you can do in Futhark, as irregular arrays (arrays where elements have
-- different length) is not allowed. However, there is a technique called
-- [Flattening by Expansion](https://futhark-lang.org/publications/array19.pdf)
-- that works. The idea is that if you have an array of `a`s, a "size function"
-- `a -> i64` that tells how many elements an `a` expands to, and a "get
-- function" `a -> i64 -> b` that computes the *i*th element of the expansion of
-- the `a`, then you can efficiently compute the final array of `b`s. The
-- precise algorithm for doing so is not important for this post - the
-- [diku-dk/segmented](https://github.com/diku-dk/segmented) package (installed
-- via [the package
-- manager](2018-08-03-the-present-futhark-package-manager.html)) provides a
-- function `expand` that we can use:

import "lib/github.com/diku-dk/segmented/segmented"

-- The type of `expand` is as follows:
--
-- ```Futhark
-- val expand [n] : (sz: a -> i64) -> (get: a -> i64 -> b) -> (arr: [n]a) -> ?[d].[d]b
-- ```
--
-- It turns out that the `num_digits` and `get_digit` functions that we defined
-- above are exactly of the form that `expand` needs (funny how that works out),
-- although we of course have to write some code for handling the other kinds of
-- expressions as well. Here is the definition of the "size" and "get" functions
-- for tokens:

def token_sz (x: token) =
  match x
  case #char _ -> 1
  case #expr (#num x) -> num_digits x
  case #expr _ -> 5

def token_get [n] (E: [n]expr) (x: token) (i: i64) : token =
  match (x, i)
  case (#char c, _) -> #char c
  case (#expr (#num x), i) -> #char (ith_digit x (num_digits x - i - 1))
  case (#expr (#op _ _ _), 0) -> #char '('
  case (#expr (#op x _ _), 1) -> #expr E[x]
  case (#expr (#op _ op _), 2) -> #char (op_char op)
  case (#expr (#op _ _ y), 3) -> #expr E[y]
  case (#expr (#op _ _ _), _) -> #char ')'

-- There is nothing fancy here: a character expands to itself, a `#num` expands
-- to one character per digit, and an `#op` expands to five new tokens:
-- enclosing parentheses, `#expr`s for its children, and its operator in the
-- middle. The only tricky thing is that the `token_get` function also needs
-- access to the full tree. Example:
--
-- ```
-- > expand token_sz (token_get expression) [#expr expression[0]]
-- [#char 40, #expr (#op 1 (#add) 2), #char 42, #expr (#num 42), #char 41]
-- ```
--
-- If we keep using `expand` until only `#char`s are left, then those characters
-- end up being the pretty-printed form of the tree. This is easy enough to put
-- together as a function:

def get_char (x: token) =
  match x
  case #char c -> c
  case _ -> '?'

def is_char (x: token) =
  match x
  case #char _ -> true
  case _ -> false

def print [n] (es: [n]expr) : []char =
  (loop (s, continue) = ([#expr es[0]], true)
   while continue do
     (expand token_sz (token_get es) s, not (all is_char s)))
  |> (.0)
  |> map get_char

-- And indeed it works:
--
-- ```
-- > :string print expression
-- ((8+20)*42)
-- ```
--
-- Although functional (in multiple ways), this solution has a big problem: we
-- are essentially traversing the tree sequentially down through the levels, so
-- a depth-*n* tree will require *n* iterations of the while loop in `print`. In
-- the [vernacular of parallel cost
-- models](2022-01-27-cost-models-are-contracts.html), we say that `print` has
-- *O(n)* *span*. This is not great, as it means that for tall and skinny trees,
-- there will be very little parallelism, except for the redundant recomputation
-- of `#char` nodes. Actually, in this case the *work* of `print` also becomes
-- quadratic, without even worrying about the parallelism issue. Clearly we
-- would like to do better.
--
-- ## Doing better
--
-- We can do better by observing how the strings output during pretty-printing
-- relate to the structure of the tree. In particular, each operator node
-- contributes the following:
--
-- 1. A *prefix string* that is printed prior to printing the left child.
--
-- 2. An *infix string* that is printed after printing the left child.
--
-- 3. A *suffix string* that is printed after printing the right child.
--
-- For our example, we can represent this as the following tree:
--
-- ![A tree rendition of our example expression. Each node has been labeled with
--  the index of the node in the array. Each inner node shows separately the
--  prefix, infix, and suffix strings. For the leaves, we can assume that they
--  have only a prefix string.](../tikz/arithmetic-tree.svg)
--
-- We can imagine that printing is done by walking this tree using a [pre-order
-- traversal](https://en.wikipedia.org/wiki/Tree_traversal#Pre-order,_NLR):
-- after printing the prefix we print the left child, and after printing the
-- infix, we print the right child. However, this is still sequential.
--
-- The final piece of the puzzle is the concept of an [Euler
-- tour](https://en.wikipedia.org/wiki/Euler_tour_technique), a technique
-- developed (as far as I know) by Tarjan and Vishkin, and which applies to a
-- great many parallel algorithms. An Euler tour describes the order in which
-- each node of a tree is first and last encountered during a pre-order
-- traversal of the tree. This diagram illustrated the Euler tour for our
-- example:
--
-- ![The previous tree now annotated with the Euler tour. Descending edges are
--  green and ascending are red. Each Euler tour edge is labeled with the order
--  in which it is encountered.](../tikz/arithmetic-tree-euler.svg)
--
-- Euler tours can be computed efficiently in parallel - the details are a bit
-- fiddly and ultimately not important for this post. However, using the Euler
-- tour we can now sketch our a parallel pretty-printing algorithm. The idea is
-- we consider each edge in the Euler tour *in parallel*, and compute what it
-- contributes to the output - ultimately we will use `expand` to stitch these
-- contributions together. The algorithm is as follows:
--
-- - When entering a node (green edge):
--   - If not first child and not the root:
--       - Print infix string of parent.
--   - Print prefix string of node.
--
-- - When leaving a node (red edge):
--   - Print suffix string of node.
--
-- There are other ways to define this algorithm, but the important part is to
-- distribute the responsibility for the various prefix/infix/suffix parts among
-- the edges of the Euler tour. Now let us see what this algorithm looks like
-- when implemented in Futhark.

-- ## Implementation

-- We will use the `euler_tour` function from
-- [diku-dk/containers](https://github.com/diku-dk/containers), which computes
-- the Euler tour of a tree given its *parent vector*:

import "lib/github.com/diku-dk/containers/tree/euler_tour"

-- ```Futhark
-- val euler_tour [n] : (P: [n]ptr) -> [n](i64, i64)
-- ```
--
-- As the name suggests, the parent vector representation of a tree is an array
-- that for each node stores the parent of that node, such that `P[p]` is the
-- parent of node `p`, and the root is its own parent. It is a fairly common
-- representation in data parallel tree algorithms. We can easily enough
-- construct the parent vector for an expression tree, by having the parent
-- [scatter](../examples/gather-and-scatter.html) its own index to the indexes
-- of its children (if any):

def mk_P [n] (es: [n]expr) =
  let f (p: i64) (e: expr) =
    match e
    case #num _ -> [(-1, p), (-1, p)]
    case #op x _ y -> [(x, p), (y, p)]
  let (is, ps) = unzip (flatten (map2 f (iota n) es))
  in scatter (iota n) is ps

-- ```
-- > mk_P expression
-- [0, 3, 3, 0, 0]
-- > euler_tour (mk_P expression)
-- [(0, 9), (2, 3), (4, 5), (1, 6), (7, 8)]
-- ```

-- The Euler tour we get isn't quite in the format we need: for each node, it
-- indicates when it is first and last encountered during the Euler tour,
-- whereas we would like a tour of node indexes, paired with whether we are
-- going down to the node or up from the node. Fortunately, this is easily
-- remedied:

type dir = #enter | #leave

def order_tour [n] (tour: [n](i64, i64)) : [2 * n](i64, dir) =
  let (lp, rp) = unzip tour
  in scatter (rep (-1, #enter))
             (lp ++ rp)
             (zip (iota n) (rep #enter) ++ zip (iota n) (rep #leave))

-- ```
-- > order_tour (euler_tour (mk_P expression))
-- [(0, #enter), (3, #enter), (1, #enter), (1, #leave), (2, #enter),
--  (2, #leave), (3, #leave), (4, #enter), (4, #leave), (0, #leave)]
-- ```

-- What remains is to build up appropriate "size" and "get" functions such that
-- we can perform an `expand` of the Euler tour. Unfortunately this is a bit
-- tedious. Out of misguided modularity, I define separate size and get
-- functions for prefix, suffix, and infix strings for nodes.

def prefix_sz (e: expr) : i64 =
  match e
  case #num x -> num_digits x
  case #op _ _ _ -> 1

def prefix_get (e: expr) (i: i64) : char =
  match e
  case #num x -> (ith_digit x (num_digits x - i - 1))
  case #op _ _ _ -> '('

def suffix_sz (e: expr) : i64 =
  match e
  case #num _ -> 0
  case #op _ _ _ -> 1

def suffix_get (e: expr) (_i: i64) : char =
  match e
  case #num _ -> '?'
  case #op _ _ _ -> ')'

def infix_sz (e: expr) : i64 =
  match e
  case #num _ -> 0
  case #op _ _ _ -> 1

def infix_get (e: expr) (_i: i64) : char =
  match e
  case #num _ -> '?'
  case #op _ op _ -> op_char op

-- We also need to have a way of determining whether a `#leave` edge should
-- print the infix of the parent of its origin - which is exactly when it is not
-- the first child, and also not the parent.

def is_first_child [n] (E: [n]expr) (P: [n]ptr) (p: ptr) =
  match E[P[p]]
  case #op x _ _ -> x == p
  case _ -> false

def print_parent_infix [n] (E: [n]expr) (P: [n]ptr) (p: ptr) =
  !is_first_child E P p && P[p] != p

-- Finally we can define a size function for an edge in the Euler tour, which
-- also needs access to the expression and parent arrays.

def euler_sz [n] (E: [n]expr) (P: [n]ptr) (p: ptr, dir: dir) =
  match dir
  case #enter ->
    prefix_sz E[p]
    + if print_parent_infix E P p then infix_sz E[P[p]] else 0
  case #leave ->
    suffix_sz E[p]

-- And similarly for the get function.

def euler_get [n] (E: [n]expr) (P: [n]ptr) (p: ptr, dir: dir) (j: i64) : char =
  match (dir, print_parent_infix E P p)
  case (#enter, true) ->
    if j < infix_sz E[P[p]]
    then infix_get E[P[p]] j
    else prefix_get E[p] (j - infix_sz E[P[p]])
  case (#enter, false) ->
    prefix_get E[p] j
  case (#leave, _) ->
    suffix_get E[p] j

-- Finally we can put together everything and define a pretty-printing function
-- that first computes the parent vector for the expression, then the Euler
-- tour, and then performs an expansion.

def print_euler [n] (E: [n]expr) =
  let P = mk_P E
  let tour = order_tour (euler_tour P)
  in expand (euler_sz E P) (euler_get E P) tour

-- ```
-- > :string print_euler expression
-- ((8+20)*42)
-- ```

-- This function has linear overall work (as a pretty-printer should), and a
-- span logarithmic in the size of the output, which is probably the best we can
-- hope for.
--
-- Overall, I am quite pleased with this result. It was a fun challenge - enough
-- to require thought, but not so hard to be frustrating, and the code is
-- actually quite nice (once you adjust your expectations to the standards of a
-- language like Futhark). The use of Euler tours is inspired by work by [Martin
-- Elsman](https://elsman.com/) and [William Henrich
-- Due](https://williamdue.github.io/) who read about them in Guy Blelloch's
-- work on data parallel algorithms, and pushed further along by Aziz Rmadi,
-- Elias Smedegaard, and Thomas Bonde Hansen, who worked on them in our [course
-- on Data Parallel Programming](https://github.com/diku-dk/dpp-e2025-pub) last
-- year (and in particular implemented the crucial conversion from parent vector
-- to Euler tour).
--
-- This pretty-printer is obviously quite limited, and I think I will spend some
-- time trying to address the remaining limitations:
--
-- 1. Minimal insertion of parentheses can probably be done by propagating a
--    "binding environment" using some of the operations Blelloch describes for
--    vector trees. Since the main task in constructing a vector tree is to
--    compute the Euler tour, which we do anyway, this should not make things
--    much more complicated.
--
-- 2. Extension to n-ary trees is in some sense trivial (you can always split
--    them to form binary trees), but in order to construct a generally useful
--    pretty-printer, some more direct solution is probably necessary. The basic
--    printing algorithm is actually not sensitive to whether the tree is binary
--    (and neither are Euler tours), so this is mostly a question of how to
--    design a nice API.
--
-- 3. Speaking of library APIs, it would be nice to make the pretty-printer
--    generic, so it can be easily instantiated with new tree types.
--
-- Overall, I think the algorithmic issues are largely solved (except item 1
-- above), and the main question is now how to expose pretty-printing as a
-- reusable library - a problem that was solved decades ago in other languages,
-- but the field of data paralellism is a wonderful place to be if you enjoy
-- turning easy problems into hard problems...
