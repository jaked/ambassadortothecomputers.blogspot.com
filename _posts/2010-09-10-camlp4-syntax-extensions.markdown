---
layout: post
title: Reading Camlp4 part 11, syntax extensions
---

In this final (?) post in my series on Camlp4, I want at last to cover
syntax extensions. A nontrivial syntax extension involves almost all
the topics we have [previously covered](/p/reading-camlp4.html), so it
seems fitting that we treat them last.

<b>Extending grammars</b>

In the post on [parsing](/2010/05/reading-camlp4-part-6-parsing.html)
we covered Camlp4 grammars but stopped short of explaining how to
extend them. Well, this is not completely true: we used the `EXTEND`
form to extend an empty grammar, and we can also use it to extend
non-empty grammars. We saw a small example of this when implementing
[quotations](/2010/08/reading-camlp4-part-8-implementing.html), where
we extended the JSON grammar with a new `json_eoi` entry (which
refered to an entry in the original grammar). Rules and levels may
also be added to existing entries, and rules may be deleted.

Let's look at a complete syntax extension, which demonstrates
modifying Camlp4's OCaml grammar. The purpose of the extension is to
change the precedence of the method call operator `#` to make "method
chaining" read better. For example, if the `foo` method returns an
object, you can write

{% highlight ocaml %}
  obj#foo "bar" #baz
{% endhighlight %}

to call the `baz` method, rather than needing

{% highlight ocaml %}
  (obj#foo "bar")#baz
{% endhighlight %}

(I originally wrote this for
use with the
[`jQuery` binding for `ocamljs`](http://github.com/jaked/ocamljs/tree/master/src/jquery/);
method chaining is common with `jQuery`.)

Here is the extension:

{% highlight ocaml %}
  open Camlp4
  
  module Id : Sig.Id =
  struct
    let name = "pa_jquery"
    let version = "0.1"
  end
  
  module Make (Syntax : Sig.Camlp4Syntax) =
  struct
    open Sig
    include Syntax
  
    DELETE_RULE Gram expr: SELF; "#"; label END;
  
    EXTEND Gram
      expr: BEFORE "apply"
        [ "#" LEFTA
          [ e = SELF; "#"; lab = label ->
              <:expr< $e$ # $lab$ >> ]
        ];
    END
  end
  
  module M = Register.OCamlSyntaxExtension(Id)(Make)
{% endhighlight %}

To make sense of a syntax extension it's helpful to refer to
`Camlp4OCamlRevisedParser.ml` (which defines the revised syntax
grammar) and `Camlp4OCamlParser.ml` (which defines the original syntax
as an extension of the revised syntax). There we see that the `#`
operator is parsed in the `expr` entry, in a level called "`.`" (which
includes other dereferencing operators), and that this level appears
below the `apply` level, which parses function application. Recall
from the [parsing](/2010/05/reading-camlp4-part-6-parsing.html) post
that operators in lower levels bind more tightly. So to get the effect
we want, we need to move the `#` rule above the `apply` level in the
grammar.

First we delete the rule from its original location: `DELETE_RULE`
takes the grammar, the entry, and the symbols on the left-hand side of
the rule, followed by `END`; you don't have to say in what level it
appears. Then we add the rule at a new location: we create a new level
`#` containing the rule from the original grammar, and add it before
the level named `apply`.

There are several ways to specify where a level is inserted: `BEFORE`
*level* and `AFTER` *level* put it before or after some other level;
`LEVEL` *level* adds rules to an existing level (you will be warned
but not stopped from changing the label or associativity of the
level); `FIRST` and `LAST` put the level before or after all other
levels. If you don't specify, rules are added to the topmost level in
the entry. The resulting grammar works just as if you had given it all
at once, making the insertions in the specified places. (However, it
is not very clear from the code how ordering works when inserting
rules into an existing level; it is perhaps best not to rely on the
order of rules in a level anyway.)

Finally we register the extension. The `Make` argument to
`OCamlSyntaxExtension` returns a `Sig.Camlp4Syntax` for some reason
(in `Register.ml` it is just ignored) so we `include Syntax` to
provide it.

(The complete code for this example is
[here](http://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/camlp4-syntax-extensions/pa_jquery).)

<b>Transforming the AST</b>

Let's do a slightly more complicated example involving some
transformation of the parsed AST. It often comes up that we want to
`let`-bind the value of an expression to a name, trapping exceptions,
then evaluate the body of the `let` outside the scope of the exception
handler. This is a bit painful to write in stock OCaml; we can only
straightforwardly express trapping exceptions in the whole `let`
expression:

{% highlight ocaml %}
  try let x = e1 in e2
  with e -> h
{% endhighlight %}

A nice alternative is to use thunks to delay the evaluation of the
body, doing it outside the scope of the `try`/`with`:

{% highlight ocaml %}
  (try let x = e1 in fun () -> e2
   with e -> fun () -> h)()
{% endhighlight %}

(We must thunkify the exception handler to make the types work out.)
This is simple enough to do by hand, but let's give it some syntactic
sugar:

{% highlight ocaml %}
  let try x = e1 in e2
  with e -> h
{% endhighlight %}

which should expand to the thunkified version above. (The idea and
syntax are taken from Martin Jambon's
[micmatch](http://martin.jambon.free.fr/micmatch.html) extension.)

Let's look at the existing rules in `Camlp4OCamlRevisedParser.ml` for
`let` and `try` to get an idea of how to parse the `let`/`try` form:

{% highlight ocaml %}
  [ "let"; r = opt_rec; bi = binding; "in"; x = SELF ->
      <:expr< let $rec:r$ $bi$ in $x$ >>
  ...
  | "try"; e = sequence; "with"; a = match_case ->
      <:expr< try $mksequence' _loc e$ with [ $a$ ] >>
{% endhighlight %}

For `let`, the `opt_rec` entry parses an optional `rec` keyword (we
see there is a special antiquotation for interpolating `rec`).
`Binding` parses a group of bindings separated by `and`. `SELF` is
just `expr`. For `try`, `sequence` is a sequence of expressions
separated by `;`, and `match_case` is a group of match cases separated
by `|`. (These entries are both a little different in the original
syntax, to account for the different semicolon rules and the `[]`
delimiters around the match cases.) Recall that
`Camlp4OCamlRevisedParser.ml` uses the revised syntax quotations, so
we have `[]` around the match cases. The call to `mksequence'` just
wraps a `do {}` around a sequence if necessary; more on this below.

The parsing rule we want is a combination of these. Here is the
extension:

{% highlight ocaml %}
  EXTEND Gram
    expr: LEVEL "top" [
      [ "let"; "try"; r = opt_rec; bi = binding; "in";
        e = sequence; "with"; a = match_case ->
          let a =
            List.map
              (function
                 | <:match_case< $p$ when $w$ -> $e$ >> ->
                     <:match_case<
                       $p$ when $w$ -> fun () -> $e$
                     >>
                 | mc -> mc)
              (Ast.list_of_match_case a []) in
          <:expr<
            (try let $rec:r$ $bi$ in fun () -> do { $e$ }
             with [ $list:a$ ])()
          >>
      ]
    ];
  END
{% endhighlight %}

We put `rec` after `try` (following `micmatch`), which is a little
weird, but if we put it before we would need to look ahead to
disambiguate `let` from `let try`; once we parse `opt_rec` we are
committed to one rule or the other. After `in` we parse `sequence`
rather than `SELF`; this seems like a good choice because there is a
`with` to end the sequence.

Now, to transform the AST, we map over the match cases. The
`match_case` entry returns a list of cases separated by `Ast.McOr`; we
call `list_of_match_case` to get an ordinary list. For each case, we
match the pattern, `when` clause, and expression on the right-hand
side (these are packaged in an `Ast.McArr`, where the `when` clause
field is `Ast.ExNil` if there is no `when` clause), and return it with
the expression thunkified. Then we return the whole `let` inside
`try`, with the body sequence thunkified.

We have to add a `do {}` around the body, creating an `Ast.ExSeq`
node, because that's what is expected by
`Camlp4Ast2OCamlAst.ml`---recall from the
[filters](/2010/03/reading-camlp4-part-5-filters.html) post that the
Camlp4 AST is translated to an OCaml AST and marshalled to the
compiler. If we forget this (and "we" often forget these
idiosyncrasies) then we get the error
"`expr; expr: not allowed here, use do {...} or [|...|] to surround them`",
which is pretty helpful as these errors go.

(The complete code for this example is
[here](http://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/camlp4-syntax-extensions/pa_let_try).)

<b>Extending pattern matching</b>

As a final example, let's extend OCaml's pattern syntax. In the
[quotations](/2010/08/reading-camlp4-part-9-implementing.html) post we
noted that JSON quotations in a pattern are not very useful, because
we would usually like a pattern to match even if the fields of an
object come in a different order or there are extra fields. To keep
the code short let's abstract the problem a little and consider
matching association lists: if we write a match case

{% highlight ocaml %}
  | alist [ "foo", x; "bar", y ] -> e
{% endhighlight %}

we would like it to match association lists with `"foo"` and `"bar"`
keys, in any order, with any extra pairs in the list. Our translation
looks like this:

{% highlight ocaml %}
  | __pa_alist_patt_1 when
      (match ((try Some (List.assoc "foo" __pa_alist_patt_1)
               with | Not_found -> None),
              (try Some (List.assoc "bar" __pa_alist_patt_1)
               with | Not_found -> None))
       with
       | (Some x, Some y) -> true
       | _ -> false)
      ->
      (match ((try Some (List.assoc "foo" __pa_alist_patt_1)
               with | Not_found -> None),
              (try Some (List.assoc "bar" __pa_alist_patt_1)
               with | Not_found -> None))
       with
       | (Some x, Some y) -> e
       | _ -> assert false)
{% endhighlight %}

This might seem overcomplicated, and it is true that we could simplify
it for this case. But the built-in pattern syntax is complicated, and
it is tricky handling all the cases to make things work smoothly; the
strategy that produces the code above will handle some (but not all)
of the complications. (We'll consider some improvements below.)

The basic idea is that when we come to an `alist` we replace it with a
new fresh name, then do further matching in a `when` clause, so if it
fails we can continue to the next case by returning `false`. In the
`when` clause we look up the keys, putting them in `option`s, then
match on the `option`s; we handle nested patterns (to the right of a
key) by embedding them in the `when` clause match. The `when` clause
match also binds variables appearing in the original pattern, so they
are available to the `when` clause of the original case (if there is
one). Finally, we do the whole thing over again in the match case body
to provide bindings to the original body.

In order to implement this we'll use both a syntax extension and a
filter. The reason is that we'd like to extend the `patt` entry, but
to do the AST transformation we sketched above we need to transform
`match_case`s. We could replace the `match_case` part of the parser as
well but that seems needlessly hairy, and generally when writing a
syntax extension we'd like to touch as little of the parser as
possible so it interoperates well with other extensions.

First, here is the syntax extension:

{% highlight ocaml %}
  EXTEND Gram
    patt: LEVEL "simple"
    [[
       "alist"; "[";
         l =
           LIST0
             [ e = expr LEVEL "simple"; ",";
               p = patt LEVEL "simple" ->
                 Ast.PaOlbi (_loc, "", p, e) ]
             SEP ";";
       "]" ->
         <:patt< $uid:"alist"$ $Ast.paSem_of_list l$ >>
    ]];
  END
{% endhighlight %}

We extend the `simple` level of the `patt` entry, which parses
primitive patterns. Inside `alist []` we parse a list of `expr` /
`patt` pairs; we parse `expr` at the `simple` level or else it would
parse the whole pair as an `expr`, and the same for `patt` just in
case. Then we return the pair of expression and pattern in an
`Ast.PaOlbi` (ordinarily used for optional argument defaults in
function definitions). Why? Well, we need to return something of type
`patt`, but we need somehow to get the `expr` to our filter, and this
is the only `patt` constructor that holds an `expr`. (As an
alternative we could parse a `patt` instead of an `expr`, but then
we'd need to translate it to an `expr` at the point we use it.)
Finally we return the list wrapped in a data constructor so we can
recognize it easily in the filter; because it is lower-case, we can be
sure that "alist" is not the identifier of a real data constructor.

Now let's look at the filter. First, some helper functions:

{% highlight ocaml %}
  let fresh =
    let id = ref 0 in
    fun () ->
      incr id;
      "__pa_alist_patt_"  ^ string_of_int !id

  let expr_tup_of_list _loc = function
    | [] -> <:expr< () >>
    | [ v ] -> v
    | vs -> <:expr< $tup:Ast.exCom_of_list vs$ >>

  let patt_tup_of_list _loc = function
    | [] -> <:patt< () >>
    | [ p ] -> p
    | ps -> <:patt< $tup:Ast.paCom_of_list ps$ >>
{% endhighlight %}

We have a function to generate fresh names, a function to turn a list
of expressions into a tuple, and a similar function for patterns. The
reason we need these latter two is that a tuple with 0 or 1 elements
is not allowed by `Camlp4Ast2OCamlAst.ml` (the empty "tuple" is
actually a special identifier in the Camlp4 AST). Next, the main
rewrite function:

{% highlight ocaml %}
  let rewrite _loc p w e =
    let k = ref (fun s f -> s) in
{% endhighlight %}

The function takes the parts of an `Ast.McArr` (that is, a match
case). We're going to map over the pattern `p`, building up a function
`k` as we encounter nested `alist` forms.  We want to generate the
same matching code in the `when` clause and the body, so `k` is
parameterized with an expression in case of success (the original
`when` clause or the body) and in case of failure (`false` or `assert
false`). We will build `k` from the inside out, starting with a
function that just returns the success expression.

{% highlight ocaml %}
    let map =
      object
        inherit Ast.map as super

        method patt p =
          match super#patt p with
            | <:patt< $uid:"alist"$ $l$ >> ->
                let id = fresh () in
                let l =
                  List.map
                    (function
                       | Ast.PaOlbi (_, _, p, e) -> p, e
                       | _ -> assert false)
                    (Ast.list_of_patt l []) in
                let vs =
                  List.map
                    (fun (_, e) ->
                       <:expr<
                         try Some (List.assoc $e$ $lid:id$)
                         with Not_found -> None
                       >>)
                    l in
                let ps =
                  List.map
                    (fun (p, _) -> <:patt< Some $p$ >>)
                    l in
                let k' = !k in
                k :=
                  (fun s f ->
                     <:expr<
                       match $expr_tup_of_list _loc vs$ with
                         | $patt_tup_of_list _loc ps$ -> $k' s f$
                         | _ -> $f$
                     >>);
                <:patt< $lid:id$ >>
            | p -> p
      end in
{% endhighlight %}

The `Ast.map` object provides methods to map each syntactic class of
the AST, along with default implementations which return the node
unchanged. We extend it to walk over the pattern, leaving it unchanged
except when we come to our special `alist` constructor. In that case
we generate a fresh name, which we return as the value of the
function. Then we extract the `expr` / `patt` pairs and map them to
`try Some (List.assoc ...` expressions and `Some` patterns. Finally we
extend `k` by matching all the expressions against all the patterns;
if the match succeeds we call the previous `k`, otherwise the failure
expression. Since we build `k` from the inside out, we transform
subpatterns first (by matching over `super#patt p`).

{% highlight ocaml %}
    let p = map#patt p in
    let w = match w with <:expr< >> -> <:expr< true >> | _ -> w in
    let w = !k w <:expr< false >> in
    let e = !k e <:expr< assert false >> in
    <:match_case< $p$ when $w$ -> $e$ >>
{% endhighlight %}

We call `map#patt` on `p` to replace special `alist` constructor nodes
with fresh names and build up `k`, then call the resulting `k` on the
`when` clause (if there is no `when` clause we replace it with `true`)
and body, and finally return the result as a `match_case`, completing
the rewrite function.

{% highlight ocaml %}
  let filter =
    let map =
      object
        inherit Ast.map as super

        method match_case mc =
          match super#match_case mc with
            | <:match_case@_loc< $p$ when $w$ -> $e$ >> ->
                rewrite _loc p w e
            | e -> e
      end in
    map#str_item

  let _ = AstFilters.register_str_item_filter filter
{% endhighlight %}

We extend `Ast.map` again to call the rewrite function on each
`match_case`, then register the resulting filter.

The code above handles `when` clauses and nested `alist` patterns, and
interacts properly with ordinary OCaml patterns. However, it
completely falls down on nested pattern alternatives. If we write

{% highlight ocaml %}
match x with
  | alist [ "foo", f ]
  | alist [ "fooz", f ] -> e
{% endhighlight %}

we get this mess:

{% highlight ocaml %}
  | __pa_alist_patt_1 | __pa_alist_patt_2 when
      (match try Some (List.assoc "fooz" __pa_alist_patt_2)
             with | Not_found -> None
       with
       | Some f ->
           (match try Some (List.assoc "foo" __pa_alist_patt_1)
                  with | Not_found -> None
            with
            | Some f -> true
            | _ -> false)
       | _ -> false)
      ->
      ... (* the same mess for the body *)
{% endhighlight %}

The first problem is that both arms of an alternative must bind the
same names, but we have replaced them with two different fresh
names. The second problem is that we have blindly treated alternative
`alist` patterns as being nested one inside the other. A solution to
both these problems is to split nested alternatives into separate
cases, at the cost of duplicating the `when` clause and body in each.

Jeremy Yallop's [patterns](http://code.google.com/p/ocaml-patterns)
framework (see [here](http://github.com/jaked/patterns) for an update
that works with OCaml 3.12.0) allows multiple pattern extensions to
coexist, and provides some common facilities to make them easier to
write. In particular it splits nested alternatives into separate
cases. Another deficiency in the code above is that it duplicates the
match expression (built in `k`) in the `when` clause and body. This
can be avoided by computing the body within the `when` clause, setting
a reference, and dereferencing it in the body. However, the reference
must be bound outside the `match_case` to be visible both in the
`when` clause and the body, so this approach must transform each AST
node that contains `match_case`s in order to bind the refs in the
right place. The `patterns` framework handles this as well.

(The complete code for this example is
[here](http://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/camlp4-syntax-extensions/pa_alist_patt).
A version using the `patterns` framework is [here](http://github.com/jaked/patterns/blob/master/applications/alist/pa_alist.ml).)
