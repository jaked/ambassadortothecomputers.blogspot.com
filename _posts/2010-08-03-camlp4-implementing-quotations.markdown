---
layout: post
title: Reading Camlp4 part 8, implementing quotations
---

The Camlp4 system of quotations and antiquotations is an awesome tool
for [producing](/2009/01/reading-camlp4-part-2-quotations_04.html) and
[consuming](/2009/01/reading-camlp4-part-4-consuming-ocaml.html) OCaml
ASTs. In this post (and the following one) we will see how to provide
this facility for other syntaxes and ASTs. Here we consider just
quotations; we'll add antiquotations in the following post.

<b>An AST for JSON</b>

Our running example will be a quotation expander for
[JSON](http://www.ietf.org/rfc/rfc4627.txt). Let's begin with the JSON
AST, in a module `Jq_ast`:

{% highlight ocaml %}
  type t =
    | Jq_null
    | Jq_bool   of bool
    | Jq_number of float
    | Jq_string of string
    | Jq_array  of t list
    | Jq_object of (string * t) list
{% endhighlight %}

This is the same (modulo order and names) as `json_type` from the
[json-wheel](http://martin.jambon.free.fr/json-wheel.html) library,
but for various reasons we will not be able to use `json_type`. The
`Jq_` prefix is for `json_quot`, the name of this little library.

<b>Parsing JSON</b>

We'll use a Camlp4
[grammar](/2010/05/reading-camlp4-part-6-parsing.html) to parse JSON
trees. It is not necessary to use Camlp4's parsing facilities in order
to implement quotations---ultimately we will need to provide just a
function from strings to ASTs, so we could use `ocamlyacc` or
what-have-you instead---but it is convenient. Here is the parser:

{% highlight ocaml %}
  open Camlp4.PreCast
  open Jq_ast
  
  module Gram = MakeGram(Lexer)
  let json = Gram.Entry.mk "json"
  
  ;;
  
  EXTEND Gram
    json: [[
        "null" -> Jq_null
      | "true" -> Jq_bool true
      | "false" -> Jq_bool false
      | i = INT -> Jq_number (float_of_string i)
      | f = FLOAT -> Jq_number (float_of_string f)
      | s = STRING -> Jq_string s
      | "["; es = LIST0 json SEP ","; "]" -> Jq_array es
      | "{";
          kvs =
            LIST0
              [ s = STRING; ":"; j = json -> (s, j) ]
              SEP ",";
        "}" -> Jq_object kvs
    ]];
  END
{% endhighlight %}

We use the default Camlp4 lexer (with `MakeGram(Lexer)`); as we have
seen, keywords mentioned in a Camlp4 grammar are added to the lexer,
so we don't need to do anything special to lex `null` etc. However,
while JSON/Javascript has a single number type, the default lexer
returns different tokens for `INT` and `FLOAT` numbers, so we convert
each to `Jq_number`. In fact, these tokens (along with `STRING`)
represent OCaml
[integer](http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#integer-literal),
[float](http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#float-literal)
and
[string](http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#string-literal)
literals, which do not exactly match the corresponding JSON ones, but
they are fairly close so let's not worry about it for now; we'll
revisit the lexer in a later post.

The parser itself is pleasingly compact; we can make good use of the
`LIST0` special symbol and an anonymous entry for parsing
objects. Unfortunately things will get a little more complicated when
we come to antiquotations.

<b>Lifting the AST</b>

Next we need to "lift" values of the JSON AST to values of the OCaml
AST. What does "lift" mean, and why do we need to do it? The goal is
to convert quotations in OCaml code, such as

{% highlight ocaml %}
  let x = <:json< [ 1, "foo", true ] >>
{% endhighlight %}

into the equivalent

{% highlight ocaml %}
  let x =
    Jq_ast.Jq_array [
      Jq_ast.Jq_number 1.;
      Jq_ast.Jq_string "foo";
      Jq_ast.Jq_bool true
    ]
{% endhighlight %}

This is to happen as part of Camlp4 preprocessing, which produces an
OCaml AST, so what we produce in place of the `<:json< ... >>`
expression must be a fragment of OCaml AST. We have a parser which
takes a valid JSON string to the JSON AST; what remains is to take a
JSON AST value to the corresponding OCaml AST. So we need a function
with cases something like:

{% highlight ocaml %}
  | Jq_null -> <:expr< Jq_null >>
  | Jq_number n -> <:expr< Jq_number $`flo:n$ >>
  | ...
{% endhighlight %}

It is not such a big deal to hand-write this lifting function for a
small AST like JSON, but it is arduous and error-prone for full-size
ASTs. Fortunately Camlp4 has a filter which does it for us. Let's
first look at the signature of the `Jq_ast` module:

{% highlight ocaml %}
  open Camlp4.PreCast
  
  type t = ... (* as above *)
  
  module MetaExpr :
  sig
    val meta_t : Ast.loc -> t -> Ast.expr
  end
  
  module MetaPatt :
  sig
    val meta_t : Ast.loc -> t -> Ast.patt
  end
{% endhighlight %}

The generated modules `MetaExpr` and `MetaPatt` provide functions to
lift a JSON AST to either an OCaml `expr` (when the quotation appears
as an expression) or `patt` (when it appears as a pattern). The `loc`
arguments are inserted into the resulting OCaml AST so that compile
errors have correct locations.

Now the implementation of `Jq_ast`:

{% highlight ocaml %}
  module Jq_ast =
  struct
    type float' = float
  
    type t = (* almost as above *)
        ...
      | Jq_number of float'
        ...
  end
  
  include Jq_ast
  
  open Camlp4.PreCast (* for Ast refs in generated code *)
  
  module MetaExpr =
  struct
    let meta_float' _loc f = <:expr< $`flo:f$ >>
    include Camlp4Filters.MetaGeneratorExpr(Jq_ast)
  end
  
  module MetaPatt =
  struct
    let meta_float' _loc f = <:patt< $`flo:f$ >>
    include Camlp4Filters.MetaGeneratorPatt(Jq_ast)
  end
{% endhighlight %}

The file needs the `Camlp4MetaGenerator` filter (the
`camlp4.metagenerator` package with `findlib`). The main idea is that
the calls to `Camlp4Filters.MetaGenerator{Expr,Patt}` are expanded
into the lifting functions. But there are a couple of fussy details:

First: The argument module `Jq_ast` which we pass to the generators is
used both on the left and right of the generated function; if you look
at the generated code there are cases like:

{% highlight ocaml %}
  | Jq_ast.Jq_null -> <:expr< Jq_ast.Jq_null >>
{% endhighlight %}

(The `<:expr< .. >>` is already expanded in the actual generated
code.) We need the AST to be available qualified by the module
`Jq_ast` both in the current file and also in code that uses the
quotation. So we have a nested `Jq_ast` module (for local uses, on the
left-hand side) which we `include` (for external uses, on the
right-hand side).

Second: The generators scan all the types defined in the current
module, then generate code from the last-appearing recursive
bundle. (In this case the recursive bundle contains just `t`, but in
general there can be more than one; mutually recursive lifting
functions are generated.) There are some special cases for predefined
types, and in particular for `float`; however, it seems to be wrong:

{% highlight ocaml %}
  let meta_float _loc s = Ast.ExFlo (_loc, s)
{% endhighlight %}

The `ExFlo` constructor takes a string representing the float, but
calls to this function are generated when you use `float` in your
type. To work around this, we define the type `float'` (on its own
rather than as part of the last-appearing recursive bundle, or else
Camlp4 would generate a `meta_float'` that calls `meta_float`), and
provide correct `meta_float'` functions. There is a similar bug with
`meta_int`, but `meta_bool` is correct, so our `Jq_bool` case does not
need fixing.

(It is interesting to contrast this approach of lifting the AST with
how it is handled in Template Haskell using the "scrap your
boilerplate" pattern; see Geoffrey Mainland's paper [Why It's Nice to
be
Quoted](http://www.eecs.harvard.edu/~mainland/publications/mainland07quasiquoting.pdf).)

<b>Quotations</b>

Finally we can hook the parser and AST lifter into Camlp4's quotation
machinery, in the `Jq_quotations` module:

{% highlight ocaml %}
  open Camlp4.PreCast
  
  module Q = Syntax.Quotation
  
  let json_eoi = Jq_parser.Gram.Entry.mk "json_eoi"
  
  EXTEND Jq_parser.Gram
    json_eoi: [[ x = Jq_parser.json; EOI -> x ]];
  END;;
  
  let parse_quot_string loc s =
    Jq_parser.Gram.parse_string json_eoi loc s
  
  let expand_expr loc _ s =
    Jq_ast.MetaExpr.meta_t loc (parse_quot_string loc s)
  
  let expand_str_item loc _ s =
    let exp_ast = expand_expr loc None s in
    <:str_item@loc< $exp:exp_ast$ >>
  
  let expand_patt loc _ s =
    Jq_ast.MetaPatt.meta_t loc (parse_quot_string loc s)
  
  ;;
  
  Q.add "json" Q.DynAst.expr_tag expand_expr;
  Q.add "json" Q.DynAst.patt_tag expand_patt;
  Q.add "json" Q.DynAst.str_item_tag expand_str_item;
  Q.default := "json"
{% endhighlight %}

First, we make a new grammar entry `json_eoi` which parses a `json`
expression followed by the end-of-input token `EOI`. Grammar entries
ordinarily ignore the rest of the input after a successful parse. If
we were to use the `json` entry directly, we would silently accept
quotations with trailing garbage, and in particular incorrect
quotations that happen to have a correct prefix, rather than alerting
the user.

Then we register quotation expanders for the `<:json< >>` quotation in
the `expr`, `patt`, and `str_item` contexts (`str_item` is useful
because that is the context at the top level prompt), using
`Syntax.Quotation.add`. All the expanders do is call the parser, then
run the result through the appropriate lifting function.

Finally we set `json` as the default quotation, so we can just say
`<< >>` for JSON quotations. This is perhaps a bit cheeky, since the user
may want something else as the default quotation; whichever module is
loaded last wins.

It is worth reflecting on how the quotation mechanism works in the
OCaml parser: There is a lexer token for quotations, but no node in
the OCaml AST, so everything must happen in the parser. When a
quotation is lexed, its entire contents is returned as a
string. (Nested quotations are matched in the lexer---see `quotation`
and `antiquot` in `camlp4/Camlpl4/Struct/Lexer.mll`---without
considering the embedded syntax; this makes the `<<` and `>>` tokens
unusable in the embedded syntax.) The string is then expanded
according to the table of registered expanders; expanders return a
fragment of OCaml AST which is inserted into the parse tree.

You might have thought (as I did) that something fancy happens with
quotations, e.g. Camlp4 switches to a different parser on the fly,
then back to the original parser for antiquotations. But it is much
simpler than that. At the same time, it is much more complicated than
that, as we will see next time when we cover antiquotations (and in
particular how nested antiquotations/quotations are handled).

(You can find the complete code
[here](http://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/camlp4-implementing-quotations),
including a pretty-printer and integration with the top level; after
building and installing you can say e.g.

{% highlight ocaml %}
  # << [ 1, "foo", true ] >>;;
  - : Jq_ast.t = [ 1, "foo", true ]
{% endhighlight %}

although without antiquotations it is not very useful.)
