---
layout: post
title: Reading Camlp4 part 5, filters
---

Hey, long time no see!

It is high time to get back to Camlp4, so I would like to pick up the
thread by covering Camlp4 *filters*. We have previously considered the
parsing and pretty-printing facilities of Camlp4 separately. But of
course the most common way to use Camlp4 is as a front-end to
`ocamlc`, where it processes files by parsing them into an AST and
pretty-printing them back to text (well, not quite---we will see below
how the AST is passed to `ocamlc`). In between we can insert filters
to transform the AST.

<b>A simple filter</b>

So let's dive into an example: a filter for type definitions that
generates `t_to_string` and `t_of_string` functions for a type `t`, a
little like Haskell's `deriving Show, Read`. To keep it simple we
handle only variant types, and only those where all the arms have no
data. Here goes:

{% highlight ocaml %}
module Make (AstFilters : Camlp4.Sig.AstFilters) =
struct
  open AstFilters
{% endhighlight %}

In order to hook into Camlp4's plugin mechanism we define the filter
as a functor. By opening `AstFilters` we get an `Ast` module in
scope. Unfortunately this is not the same `Ast` we got previously from
`Camlp4.PreCast` (although it has the same signature) so all our code
that uses `Ast` (including all OCaml syntax quotations) needs to go
inside the functor body.

{% highlight ocaml %}
  let rec filter si =
    match wrap_str_item si with
      | <:str_item< type $lid:tid$ = $Ast.TySum (_, ors)$ >> ->
          begin
            try
              let cons =
                List.map
                  (function
                     | <:ctyp< $uid: c$ >> -> c
                     | _ -> raise Exit)
                  (Ast.list_of_ctyp ors []) in
              to_of_string si tid cons
            with Exit -> si
          end
       | _ -> si
{% endhighlight %}

The `filter` function filters `Ast.str_item`s. (It is not actually
recursive but we say `let rec` so we can define helper functions
afterward). If a `str_item` has the right form we transform it by
calling `to_of_string`, otherwise we return it unchanged. We match a
sum type definition, then extract the constructor names (provided that
they have no data) into a string list. (Recall that a `TySum` contains
arms separated by `TyOr`; the call to `list_of_ctyp` converts that to
a list of arms.)

{% highlight ocaml %}
  and wrap_str_item si =
    let _loc = Ast.loc_of_str_item si in
    <:str_item< $si$ >>
{% endhighlight %}

For some reason, `<:str_item< $si$ >>` wraps an extra `StSem` /
`StNil` around `si`, so in order to use the quotation syntax on the
left-hand side of a pattern match we need to do the same wrapping.

{% highlight ocaml %}
  and to_of_string si tid cons =
    let _loc = Ast.loc_of_str_item si in
    <:str_item<
      $si$;;
      $to_string _loc tid cons$;;
      $of_string _loc tid cons$;;
    >>
{% endhighlight %}

This `str_item` replaces the original one in the output, so we include
the original one in additional to new ones containing the
`t_to_string` and `t_of_string` functions.

{% highlight ocaml %}
  and to_string _loc tid cons =
    <:str_item<
      let $lid: tid ^ "_to_string"$ = function
        $list:
          List.map
            (fun c -> <:match_case< $uid: c$ -> $`str: c$ >>)
            cons$
    >>
{% endhighlight %}

To convert a variant to a string, we match over its constructors and
return the corresponding string.

{% highlight ocaml %}
  and of_string _loc tid cons =
    <:str_item<
      let $lid: tid ^ "_of_string"$ = function
        $list:
          List.map
            (fun c -> <:match_case<
	      $tup: <:patt< $`str: c$ >>$ -> $uid: c$
	    >>)
            cons$
        | _ -> invalid_arg "bad string"
    >>
{% endhighlight %}

To convert a string to a variant, we match over the corresponding
string for each constructor and return the constructor; we also need a
catchall for strings that match no constructor. (What is this `tup`
and `patt` business? A contrived bug which we will fix below.)

{% highlight ocaml %}
  ;;

  AstFilters.register_str_item_filter begin fun si ->
    let _loc = Ast.loc_of_str_item si in
    <:str_item<
      $list: List.map filter (Ast.list_of_str_item si [])$
    >>
  end
{% endhighlight %}

Now we register our filter function with Camlp4. The input `str_item`
may contain many `str_items`s separated by `StSem`, so we call
`list_of_str_item` to get a list of individuals.

{% highlight ocaml %}
end

module Id =
struct
  let name = "to_of_string"
  let version = "0.1"
end

;;

let module M = Camlp4.Register.AstFilter(Id)(Make) in ()
{% endhighlight %}

Finally we register the plugin with Camlp4. The functor application is
just for its side effect, so the plugin is registered when its `.cmo`
is loaded. We can compile the plugin with

{% highlight bash %}
ocamlfind ocamlc -package camlp4.quotations.o -syntax camlp4o \
  -c to_of_string.ml
{% endhighlight %}

and run it on a file (containing `type t = Foo | Bar | Baz` or
something) with

{% highlight bash %}
camlp4o to_of_string.cmo test.ml
{% endhighlight %}

<b>Ocamlc's AST</b>

Looks pretty good, right? But something goes wrong when we try to use
our plugin as a frontend for `ocamlc`:

{% highlight bash %}
ocamlc -pp 'camlp4o ./to_of_string.cmo' test.ml
{% endhighlight %}

We get a preprocessor error, "singleton tuple pattern". It turns out
that Camlp4 passes the processed AST to `ocamlc` not by
pretty-printing it to text, but by converting it to the AST type that
`ocamlc` uses and marshalling it. This saves the time of reparsing it,
and also passes along correct file locations (compare to `cpp`'s
`#line` directives). However, as we have seen, the Camlp4 AST is
pretty loose. When converting to an `ocamlc` AST, Camlp4 does some
validity checks on the tree. What can be confusing is that an AST that
fails these checks may look fine when pretty-printed.

Here the culprit is the line

{% highlight ocaml %}
	      $tup: <:patt< $`str: c$ >>$ -> $uid: c$
{% endhighlight %}

which produces an invalid pattern consisting of a one-item tuple. When
pretty-printed, though, the `tup` just turns into an extra set of
parentheses, which `ocamlc` doesn't mind. What we wanted was

{% highlight ocaml %}
	      $`str: c$ -> $uid: c$
{% endhighlight %}

This is a contrived example, but this kind of error is easy to make,
and can be hard to debug, because looking at the pretty-printed output
doesn't tell you what's wrong. One tactic is to run your code in the
toplevel, which will print the constructors of the AST as
usual. Another is to use a filter that comes with Camlp4 to "lift" the
AST---that is, to generate the AST representing the original AST!
Maybe it is easier to try it than to explain it:

{% highlight bash %}
camlp4o to_of_string.cmo -filter Camlp4AstLifter test.ml
{% endhighlight %}

Now compare the result to the tree you get back from Camlp4's parser
for the code you *meant* to write, and you can probably spot your
mistake.

(If you tried to redirect the `camlp4o` command to a file or pipe it
through `less` you got some line noise---this is the marshalled
`ocamlc` AST. By default Camlp4 checks whether its output is a TTY; if
so it calls the pretty-printer, if not the `ocamlc` AST marshaller. To
override this use the `-printer o` option, or `-printer r` for revised
syntax.)

<b>Other builtin filters</b>

This `Camlp4AstLifter` is pretty useful. What else comes with Camlp4?
There are several other filters in `camlp4/Camlp4Filters` which you
can call with `-filter`:

* `Camlp4FoldGenerator` generates visitor classes from datatypes. Try
  putting `class x = Camlp4MapGenerator.generated` after a type
  definition. The idea is that you can override methods of the visitor
  so you can do some transformation on a tree without having to write
  the boilerplate to walk the parts you don't care about. In fact,
  this filter is used as part of the Camlp4 bootstrap to generate
  vistors for the AST; you can see the `map` and `fold` classes in
  `camlp4/Camlp4/Sig.ml`.

* `Camlp4MetaGenerator` generates lifting functions from a type
  definition---these functions are what `Camlp4AstLifter` uses to lift
  the AST, and it's also how quotations are implemented. I'm planning
  to cover how to implement quotations / antiquotations (for a
  different language) in a future post, and `Camlp4MetaGenerator` will
  be crucial.

* `Camlp4LocationStripper` replaces all the locations in an AST with
  `Loc.ghost`. I don't know what this is for, but it might be useful
  if you wanted to compare two ASTs and be insensitive to their
  locations.

* `Camlp4Profiler` inserts profiling code, in the form of function
  call counts. I haven't tried it, and I'm not sure when you would
  want it in preference to gprof.

* `Camlp4TrashRemover` just filters out a module called
  `Camlp4Trash`. Such a module may be found in
  `camlp4/Camlp4/Struct/Camlp4Ast.mlast`; I think the idea is that the
  module is there in order to generate some stuff, but the module
  itself is not needed.

* `Camlp4MapGenerator` has been subsumed by `Camlp4FoldGenerator`.

* `Camlp4ExceptionTracer` seems to be a special-purpose tool to help debug Camlp4.

OK, maybe not too much useful stuff here, but it is interesting to
work out how Camlp4 is bootstrapped.

I think next time I will get into Camlp4's extensible parsers, on the
way toward syntax extensions.

<b>Colophon</b>

I wrote my previous posts in raw HTML, with highlighted code generated
from a hightlighted Emacs buffer by [htmlize.el] (http://fly.cc.fer.hr/~hniksic/emacs/htmlize.el).
Iterating on this setup was unutterably painful. This post was written
using [jekyll] (http://github.com/mojombo/jekyll) with a simple
template to approximate the Blogspot formatting, mostly so I can check
that lines of code aren't too long. Jekyll is very nice: you can write
text with [Markdown] (http://maruku.rubyforge.org/), and highlight
code with [Pygments] (http://pygments.org/).
