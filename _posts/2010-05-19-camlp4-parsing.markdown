---
layout: page
title: Reading Camlp4 part 6, parsing
---

In this post I want to discuss Camlp4's *stream parsers* and
*grammars*. Since the OCaml parsers in Camlp4 (which we touched on
[previously](http://ambassadortothecomputers.blogspot.com/2009/01/reading-camlp4-part-3-quotations-in.html))
use them, it's necessary to understand them in order to write syntax
extensions; independently, they are a nice alternative to `ocamlyacc`
and other parser generators. Stream parsers and grammars are outlined
for the old Camlp4 in the
[tutorial](http://caml.inria.fr/pub/docs/tutorial-camlp4/) and
[manual](http://caml.inria.fr/pub/docs/manual-camlp4/), but some of
the details have changed, and there are many aspects of grammars which
are given only a glancing treatment in that material.

<b>Streams and stream parsers</b>

Parsers generated from Camlp4 grammars are built on stream parsers, so
let's start there. It will be easier to explain grammars with this
background in hand, and we will see that it is sometimes useful to
drop down to stream parsers when writing grammars.

A *stream* of type `'a Stream.t` is a sequence of elements of type
`'a`. Elements of a stream are accessed sequentially; reading the
first element of a stream has the side effect of advancing the stream
to the next element. You can also peek ahead into a stream without
advancing it. Camlp4 provides a syntax extension for working with
streams, which expands to operations on the
[Stream](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Stream.html)
module of the standard library.

There are various ways to make a stream but we'll focus on consuming
them; for testing you can make a literal stream with the syntax
`[< '"foo"; '"bar"; '"baz" >]`---note the extra single-quotes. With
the `parser` keyword we can write a function to consume a stream by
pattern-matching over prefixes of the stream:

{% highlight ocaml %}
let rec p = parser
  | [< '"foo"; 'x; '"bar" >] -> "foo-bar+" ^ x
  | [< '"baz"; y = p >] -> "baz+" ^ y
{% endhighlight %}

The syntax `'"foo"` means match a value `"foo"`; `'x` means match any
value, binding it to `x`, which can be used on the right-hand side of
the match as usual; and `y = p` means call the parser `p` on the rest
of the stream, binding the result to `y`. You probably get the rough
idea, but let's run it through Camlp4 to see exactly what's happening:

{% highlight ocaml %}
let rec p (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some "foo" ->
      (Stream.junk __strm;
       (match Stream.peek __strm with
        | Some x ->
            (Stream.junk __strm;
             (match Stream.peek __strm with
              | Some "bar" -> (Stream.junk __strm; "foo-bar+" ^ x)
              | _ -> raise (Stream.Error "")))
        | _ -> raise (Stream.Error "")))
  | Some "baz" ->
      (Stream.junk __strm;
       let y =
         (try p __strm
          with | Stream.Failure -> raise (Stream.Error ""))
       in "baz+" ^ y)
  | _ -> raise Stream.Failure
{% endhighlight %}

We can see that "parser" is perhaps a strong word for this construct;
it's really just a nested pattern match. The generated function
`peek`s the next element in the stream, then `junk`s it once it finds
a match (advancing the stream to the next element). If there's no
match on the first token, that's a `Stream.Failure` (the stream is not
advanced, giving us the opportunity to try another parser); but once
we have matched the first token, a subsequent match failure is a
`Stream.Error` (we have committed to a branch, and advanced the
stream; if the parse fails now we can't try another parser).

A call to another parser as the first element of the pattern is
treated specially: for this input
{% highlight ocaml %}
let rec p = parser
  | [< x = q >] -> x
  | [< '"bar" >] -> "bar"
{% endhighlight %}
we get
{% highlight ocaml %}
let rec p (__strm : _ Stream.t) =
  try q __strm
  with
  | Stream.Failure ->
      (match Stream.peek __strm with
       | Some "bar" -> (Stream.junk __strm; "bar")
       | _ -> raise Stream.Failure)
{% endhighlight %}
So there is a limited means of backtracking: if `q` fails with
`Stream.Failure` (meaning that the stream has not been advanced) we
try the next arm of the parser.

It's easy to see what would happen if we were to use the same literal
as the first element of more than one arm: the first one gets the
match. Same if we were to make a recursive call (to the same parser)
as the first element: we'd get an infinite loop, since it's just a
function call. So we can't give arbitrary BNF-like grammars to
`parser`. We could use it as a convenient way to hand-write a
recursive-descent parser, but we won't pursue that idea here. Instead,
let's turn to Camlp4's grammars, which specify a recursive-descent
parser using a BNF-like syntax.

<b>Grammars</b>

Here is a complete example of a grammar:
{% highlight ocaml %}
open Camlp4.PreCast
module Gram = MakeGram(Lexer)
let expr = Gram.Entry.mk "expr"
EXTEND Gram
  expr:
    [[
       "foo"; x = LIDENT; "bar" -> "foo-bar+" ^ x
     | "baz"; y = expr -> "baz+" ^ y
     ]];
END
;;
try
  print_endline
    (Gram.parse_string expr Loc.ghost Sys.argv.(1))
with Loc.Exc_located (_, x) -> raise x
{% endhighlight %}

You can build it with the following command:
{% highlight bash %}
ocamlfind ocamlc \
   -linkpkg -syntax camlp4o \
  -package camlp4.extend -package camlp4.lib \
  grammar1.ml -o grammar1
{% endhighlight %}

Let's cover the infrastructure before investigating `EXTEND`. We have
a grammar module `Gram` which we got from `Camlp4.PreCast`; this is an
empty grammar using a default lexer. We have an *entry* (a grammar
nonterminal) `expr`, which is an OCaml value. We can parse a string
starting at an entry using `Gram.parse_string` (we have to pass it an
initial location). We trap `Loc.Exc_located` (which attaches a
location to exceptions raised in parsing) and re-raise the underlying
exception so it gets printed. (In subsequent examples I will give just
the `EXTEND` block.)

One way to approach `EXTEND` is to run the file through Camlp4
(`camlp4of` has the required syntax extension) to see what we
get. This is fun, but the result does not yield much insight; it's
just a simple transformation of the input, passed to `Gram.extend`.
This is the entry point to a pretty hairy bunch of code that generates
a recursive descent parser from the value representing the
grammar. Let's take a different tack:
[RTFM](http://caml.inria.fr/pub/docs/manual-camlp4/manual005.html),
then run some experiments to shine light in places where the fine
manual is a bit dim.

First, what language is parsed by the grammar above? It looks pretty
similar to the stream parser example. But what is `LIDENT`? The stream
parser example works with a stream of strings. Here we are working
with a stream of tokens, produced by the `Lexer` module; there is a
variant defining the token types in `PreCast.mli`. The default lexer
is OCaml-specific (but it's often good enough for other purposes); a
`LIDENT` is an OCaml lowercase identifier. A literal string (like
`"foo"`) indicates a `KEYWORD` token; using it in a grammar registers
the keyword with the lexer. So the grammar can parse strings like
`foo quux bar` or `baz foo quux bar`, but not `foo bar bar`, since
`bar` is a `KEYWORD` not a `LIDENT`.

Most tokens have associated strings; `x = LIDENT` puts the associated
string in `x`. Keywords are given in double quotes (`x = KEYWORD`
works, but I can't think of a good use for it). You can also use
pattern-matching syntax (e.g. `` `LIDENT x``) to get at the actual
token constructor, which may carry more than just a string.

You can try the example and see that the lexer takes care of
whitespace and OCaml comments. You'll also notice that the parser
ignores extra tokens after a successful parse; to avoid it we need an
`EOI` token to indicate the end of the input (but I haven't bothered
here).

<b>Left-factoring</b>

What happens if two rules start with the same token?
{% highlight ocaml %}
EXTEND Gram
  expr:
    [[
       "foo"; "bar" -> "foo+bar"
     | "foo"; "baz" -> "foo+baz"
     ]];
END
{% endhighlight %}
If this were a stream parser, the first arm would always match when
the next token is `foo`; if the subsequent token is `baz` then the
parse fails. But with a grammar, the *rule*s (arms, for a grammar) are
*left-factored*: when there is a common prefix of *symbol*s (a symbol
is a keyword, token, or entry---and we will see some others later)
among different rules, the parser doesn't choose which rule to use
until the common prefix has been parsed. You can think of a factored
grammar as a tree, where the nodes are symbols and the leaves are
*action*s (the right-hand side of a rule is the rule's action); when a
symbol distinguishes two rules, that's a branching point. (In fact,
this is how grammars are implemented: first the corresponding tree is
generated, then the parser is generated from the tree.)

What if one rule is a prefix of another?
{% highlight ocaml %}
EXTEND Gram
  expr:
    [[
       "foo"; "bar" -> "foo+bar"
     | "foo"; "bar"; "baz" -> "foo+bar+baz"
     ]];
END
{% endhighlight %}
In this case the parser is greedy: if the next token is `baz`, it uses
the second rule, otherwise the first. To put it another way, a token
or keyword is preferred over *epsilon*, the empty string (and this
holds for other ways that a grammar can match epsilon---see below
about special symbols).

What if two rules call the same entry?
{% highlight ocaml %}
EXTEND Gram
  GLOBAL: expr;

  f: [[ "quux" ]];

  expr:
    [[
       "foo"; f; "bar" -> "foo+bar"
     | "foo"; f; "baz" -> "foo+baz"
     ]];
END
{% endhighlight %}

First, what is this `GLOBAL`? By default, all entries are global,
meaning that they must be pre-defined with `Gram.Entry.mk`. The
`GLOBAL` declaration gives a list of entries which are global, and
makes the rest local, so we don't need to pre-define them, but we
can't refer to them outside the grammar. Second, note that we can call
entries without binding the result to a variable, and that rules don't
need an action---in that case they return `()`. You can try it and see
that factoring works on entries too. Maybe this is slightly
surprising, if you're thinking about the rules as parse-time
alternatives, but factoring happens when the parser is built.

What about an entry vs. a token?
{% highlight ocaml %}
EXTEND Gram
  GLOBAL: expr;

  f: [[ "baz" ]];

  expr:
    [[
       "foo"; "bar"; f -> "foo+bar"
     | "foo"; "bar"; "baz" -> "foo+bar+baz"
     ]];
END
{% endhighlight %}
Both rules parse the same language, but an explicit token or keyword
trumps an entry or other symbol, so the second rule is used. You can
try it and see that the order of the rules doesn't matter.

What about two different entries?
{% highlight ocaml %}
EXTEND Gram
  GLOBAL: expr;

  f1: [[ "quux" ]];
  f2: [[ "quux" ]];

  expr:
    [[
       "foo"; f1; "bar" -> "foo+bar"
     | "foo"; f2; "baz" -> "foo+baz"
     ]];
END
{% endhighlight %}
Factoring happens only within a rule, so the parser doesn't know that
`f1` and `f2` parse the same language. It commits to the first rule
after parsing `foo`; if after parsing `quux` it then sees `baz`, it
doesn't backtrack and try the second rule, so the parse fails. If you
switch the order of the rules, then `baz` succeeds but `bar` fails.

<b>Local backtracking</b>

Why have two identical entries in the previous example? If we make
them different, something a little surprising happens:

{% highlight ocaml %}
EXTEND Gram
  GLOBAL: expr;

  f1: [[ "quux" ]];
  f2: [[ "xyzzy" ]];

  expr:
    [[
       "foo"; f1; "bar" -> "foo+bar"
     | "foo"; f2; "baz" -> "foo+baz"
     ]];
END
{% endhighlight %}

Now we can parse both `foo quux bar` and `foo xyzzy baz`. How does
this work? It takes a little digging into the implementation (which I
will spare you) to see what's happening: the `"foo"` keyword is
factored into a common prefix, then we have a choice between `f1` and
`f2`. A choice betwen entries generates a stream parser, with an arm
for each entry which calls the entry's parser. As we saw in the stream
parsers sections, calling another parser in the first position of a
match compiles to a limited form of backtracking. So in the example,
if `f1` fails with `Stream.Failure` (which it does when the next token
is not `quux`) then the parser tries to parse `f2` instead.

Local backtracking works only when the parser is at a branch point
(e.g. a choice between two entries), and when the called entry does
not itself commit and advance the stream (in which case `Stream.Error`
is raised on a parse error instead of `Stream.Failure`). Here is an
example that fails the first criterion:
{% highlight ocaml %}
EXTEND Gram
  GLOBAL: expr;

  f1: [[ "quux" ]];
  f2: [[ "xyzzy" ]];
  g1: [[ "plugh" ]];
  g2: [[ "plugh" ]];

  expr:
    [[
       g1; f1 -> "f1"
     | g2; f2 -> "f2"
     ]];
END
{% endhighlight %}
After parsing `g1`, the parser has committed to the first rule, so
it's not possible to backtrack and try the second if `f1` fails.

Here's an example that fails the second criterion:
{% highlight ocaml %}
EXTEND Gram
  GLOBAL: expr;

  g: [[ "plugh" ]];
  f1: [[ g; "quux" ]];
  f2: [[ g; "xyzzy" ]];

  expr:
    [[ f1 -> "f1" | f2 -> "f2" ]];
END
{% endhighlight %}
When `f1` is called, after parsing `g` the parser is committed to
`f1`, so if the next token is not `quux` the parse fails rather than
backtracking.

Local backtracking can be used to control parsing with explicit
lookahead. We could repair the previous example as follows:
{% highlight ocaml %}
let test =
  Gram.Entry.of_parser "test"
    (fun strm ->
       match Stream.npeek 2 strm with
         | [ _; KEYWORD "xyzzy", _ ] -> raise Stream.Failure
         | _ -> ())
EXTEND Gram
  GLOBAL: expr;

  g: [[ "plugh" ]];
  f1: [[ g; "quux" ]];
  f2: [[ g; "xyzzy" ]];

  expr:
    [[ test; f1 -> "f1" | f2 -> "f2" ]];
END
{% endhighlight %}
We create an entry from a stream parser with
`Gram.Entry.of_parser`. This could do some useful parsing and return a
value just like any other entry, but here we just want to cause a
backtrack (by raising `Stream.Failure`) if the token *after* the next
one is `xyzzy`. We can see it with `Stream.npeek 2`, which returns the
next two tokens, but does not advance the stream. (The stream parser
syntax is not useful here since it advances the stream on a match.)
You can see several examples of this technique in `Camlp4OCamlParser.ml`.

We have seen that for stream parsers, a match of a sequence of
literals compiles to a nested pattern match; as soon as the first
literal matches, we're committed to that arm. With grammars, however,
a sequence of tokens (or keywords) is matched all at once: enough
tokens are `peek`ed; if all match then the stream is advanced past all
of them; if any fail to match, `Stream.Failure` is raised. So in the
first example of this section, `f1` could be any sequence of tokens,
and local backtracking would still work. Or it could be a sequence of
tokens followed by some non-tokens; as long as the failure happens in
the sequence of tokens, local backtracking would still work.

<b>Self-calls</b>

Consider the following grammar:
{% highlight ocaml %}
EXTEND Gram
  GLOBAL: expr;

  b: [[ "b" ]];

  expr:
    [[ expr; "a" -> "a" | b -> "b" ]];
END
{% endhighlight %}
We've seen that a choice of entries generates a stream parser with an
arm for each entry, and also that a call to another parser in a stream
parser match is just a function call. So it seems like the parser
should go into a loop before parsing anything.

However, Camlp4 gives calls to the entry being defined ("self-calls")
special treatment. The rules of an entry actually generate two
parsers, the "start" and "continue" parsers (these names are taken
from the code). When a self-call appears as the first symbol of a
rule, the rest of the rule goes into the continue parser; otherwise
the whole rule goes into the start parser. An entry is parsed starting
with the start parser; a successful parse is followed by the continue
parser. So in the example, we first parse using just the second rule,
to get things off the ground, then parse using just the first rule. If
there are no start rules (that is, all rules begin with self-calls)
the parser doesn't loop, but it fails without parsing anything.

<b>Levels and precedence</b>

I am sorry to say that I have not been completely honest with you. I
have made it seem like entries consist of a list of rules in double
square brackets. In fact, entries are lists of *level*s, in single
square brackets, and each level consists of a list of rules, also in
single square brackets. So each of the examples so far has contained
only a single level. Here is an example with multiple levels:
{% highlight ocaml %}
EXTEND Gram
  expr:
    [ [ x = expr; "+"; y = expr -> x + y
      | x = expr; "-"; y = expr -> x - y ]
    | [ x = expr; "*"; y = expr -> x * y
      | x = expr; "/"; y = expr -> x / y ]
    | [ x = INT -> int_of_string x
      | "("; e = expr; ")" -> e ] ];
END
{% endhighlight %}

(You'll need a `string_of_int` to use this grammar with the earlier
framework.) The idea with levels is that parsing begins at the topmost
level; if no rule applies in the current level, then the next level
down is tried. Furthermore, when making a self-call, call at the
current level (or the following level; see below) rather than at the
top. This gives a way to implement operator precedence: order the
operators top to bottom from loosest- to tightest-binding.

Why does this work? The multi-level grammar is just a "stratified"
grammar, with a little extra support from Camlp4; we could write it
manually like this:
{% highlight ocaml %}
EXTEND Gram
  GLOBAL: expr;

  add_expr:
    [[
       x = add_expr; "+"; y = mul_expr -> x + y
     | x = add_expr; "-"; y = mul_expr -> x - y
     | x = mul_expr -> x
     ]];

  mul_expr:
    [[
       x = mul_expr; "*"; y = base_expr -> x * y
     | x = mul_expr; "/"; y = base_expr -> x / y
     | x = base_expr -> x
     ]];

  base_expr:
    [[
       x = INT -> int_of_string x
     | "("; e = add_expr; ")" -> e
     ]];

  expr: [[ add_expr ]];
END
{% endhighlight %}
When parsing a `mul_expr`, for instance, we don't want to parse an
`add_expr` as a subexpression; `1 * 2 + 3` should not parse as
`1 * (2 + 3)`. A stratified grammar just leaves out the rules for
lower-precedence operators at each level. Why do we call `add_expr` on
the left side of `+` but `mul_expr` on the right? This makes `+`
left-associative; we parse `1 + 2 + 3` as `(1 + 2) + 3` since
`add_expr` is a possibility only on the left. (For an ordinary
recursive-descent parser we'd want right-associativity to prevent
looping, although the special treatment of self-calls makes the
left-associative version work here.)

Associativity works just the same with the multi-level grammar. By
default, levels are left-associative: in the start parser (for a
self-call as the first symbol of the rule), the self-call is made at
the same level; in the continue parser, self-calls are made at the
following level. For right-associativity it's the reverse, and for
non-associativity both start and continue parsers call the following
level. The associativity of a level can be specified by prefixing it
with the keywords `NONA`, `LEFTA`, or `RIGHTA`. (Either I don't
understand what non-associativity means, or `NONA` is broken; it seems
to be the same as `LEFTA`.)

Levels may be labelled, and the level to call may be given
explicitly. So another way to write the same grammar is:
{% highlight ocaml %}
EXTEND Gram
  expr:
    [ "add"
      [ x = expr LEVEL "mul"; "+"; y = expr LEVEL "add" -> x + y
      | x = expr LEVEL "mul"; "-"; y = expr LEVEL "add" -> x - y
      | x = expr LEVEL "mul" -> x ]
    | "mul"
      [ x = expr LEVEL "base"; "*"; y = expr LEVEL "mul" -> x * y
      | x = expr LEVEL "base"; "/"; y = expr LEVEL "mul" -> x / y
      | x = expr LEVEL "base" -> x ]
    | "base"
      [ x = INT -> int_of_string x
      | "["; e = expr; "]" -> e ] ];
END
{% endhighlight %}
(Unfortunately, the left-associative version of this loops; explicitly
specifying a level when calling an entry defeats the start / continue
mechanism, since the call is not recognized as a self-call.) Calls to
explicit levels can be used when calling other entries, too, not just
for self calls. Level names are also useful for extending grammars,
although we won't cover that here.

<b>Special symbols</b>

There are several special symbols: `SELF` refers to the entry being
defined (at the current or following level depending on the
associativity and the position of the symbol in the rule, as above);
`NEXT` refers to the entry being defined, at the following level
regardless of associativity or position.

A list of zero or more items can be parsed with the syntax `LIST0`
*elem*, where *elem* can be any other symbol. The return value has
type `'a list` when *elem* has type `'a`. To parse separators between
the elements use `LIST0` *elem* `SEP` *sep*; again *sep* can be any
other symbol. `LIST1` means parse one or more items. An optional item
can be parsed with `OPT` *elem*; the return value has type `'a
option`. (Both `LIST0` and `OPT` can match the empty string; see the
note above about the treatment of epsilon.)

Finally, a nested set of rules may appear in a rule, and acts like an
anonymous entry (but can have only one level). For example, the rule
{% highlight ocaml %}
  x = expr; ["+" | "plus"]; y = expr -> x + y
{% endhighlight %}
parses both `1 + 2` and `1 plus 2`.

***

Almost the whole point of Camlp4 grammars is that they are
extensible---you can add rules and levels to entries after the
fact---so you can modify the OCaml parsers to make syntax
extensions. But I am going to save that for a later post.
