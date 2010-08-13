---
layout: post
title: Reading Camlp4 part 10, custom lexers
---

As a final modification to our running JSON quotation example, I want
to repair a problem noted in the
[first post](/2010/08/reading-camlp4-part-8-implementing.html)---that
the default lexer does not match the
[JSON spec](http://www.ietf.org/rfc/rfc4627.txt)---and in doing so
demonstrate the use of custom lexers with Camlp4 grammars. We'll parse
UTF8-encoded Javascript using the
[ulex](http://www.cduce.org/download.html#side) library.

To use a custom lexer, we need to pass a module matching the `Lexer`
signature (in `camlp4/Camlp4/Sig.ml`) to
`Camlp4.PreCast.MakeGram`. (Recall that we get back an empty grammar
which we then extend with parser entries. ) Let's look at the
signature and its subsignatures, and our implementation of each:

<b>Error</b>

{% highlight ocaml %}
  module type Error = sig
    type t
    exception E of t
    val to_string : t -> string
    val print : Format.formatter -> t -> unit
  end
{% endhighlight %}

First we have a module for packaging up an exception so it can be
handled generically (in particular it may be registered with
`Camlp4.ErrorHandler` for common printing and handling). We have
simple exception needs so we give a simple implementation:

{% highlight ocaml %}
  module Error =
  struct
    type t = string
    exception E of string
    let print = Format.pp_print_string
    let to_string x = x
  end
  let _ = let module M = Camlp4.ErrorHandler.Register(Error) in ()
{% endhighlight %}

<b>Token</b>

Next we have a module defining the tokens our lexer supports:

{% highlight ocaml %}
  module type Token = sig
    module Loc : Loc
  
    type t
  
    val to_string : t -> string
    val print : Format.formatter -> t -> unit
    val match_keyword : string -> t -> bool
    val extract_string : t -> string
  
    module Filter : ... (* see below *)
    module Error : Error
  end
{% endhighlight %}

The type `t` represents a token. This can be anything we like (in
particular it does not need to be a variant with arms `KEYWORD`,
`EOI`, etc. although that is the conventional representation), so long
as we provide the specified functions to convert it to a string, print
it to a formatter, determine if it matches a string keyword (recall
that we can use literal strings in grammars; this function is called
to see if the next token matches a literal string), and extract a
string representation of it (called when you bind a variable to a
token in a grammar---e.g. `n = NUMBER`). Here's our implementation:

{% highlight ocaml %}
  type token =
    | KEYWORD  of string
    | NUMBER   of string
    | STRING   of string
    | ANTIQUOT of string * string
    | EOI

  module Token =
  struct
    type t = token
  
    let to_string t =
      let sf = Printf.sprintf in
      match t with
        | KEYWORD s       -> sf "KEYWORD %S" s
        | NUMBER s        -> sf "NUMBER %s" s
        | STRING s        -> sf "STRING \"%s\"" s
        | ANTIQUOT (n, s) -> sf "ANTIQUOT %s: %S" n s
        | EOI             -> sf "EOI"
  
    let print ppf x = Format.pp_print_string ppf (to_string x)
  
    let match_keyword kwd =
      function
        | KEYWORD kwd' when kwd = kwd' -> true
        | _ -> false
  
    let extract_string =
      function
        | KEYWORD s | NUMBER s | STRING s -> s
        | tok ->
            invalid_arg
              ("Cannot extract a string from this token: " ^
                 to_string tok)

    module Loc = Camlp4.PreCast.Loc
    module Error = Error
    module Filter = ... (* see below *)
  end
{% endhighlight %}

Not much to it. `KEYWORD` covers `true`, `false`, `null`, and
punctuation; `NUMBER` and `STRING` are JSON numbers and strings; as we
saw [last time](/2010/08/reading-camlp4-part-9-implementing.html)
antiquotations are returned in `ANTIQUOT`; finally we signal the end
of the input with `EOI`.

<b>Filter</b>

{% highlight ocaml %}
  module Filter : sig
    type token_filter =
      (t * Loc.t) Stream.t -> (t * Loc.t) Stream.t

    type t

    val mk : (string -> bool) -> t
    val define_filter : t -> (token_filter -> token_filter) -> unit
    val filter : t -> token_filter
    val keyword_added : t -> string -> bool -> unit
    val keyword_removed : t -> string -> unit
  end;
{% endhighlight %}

The `Filter` module provides filters over token streams. We don't have
a need for it in the JSON example, but it's interesting to see how it
is implemented in the default lexer and used in the OCaml parser. The
argument to `mk` is a function indicating whether a string should be
treated as a keyword (i.e. the literal string is used in the grammar),
and the default lexer uses it to filter the token stream to convert
identifiers into keywords. If we wanted the JSON parser to be
extensible, we would need to take this into account; instead we'll
just stub out the functions:

{% highlight ocaml %}
  module Filter =
  struct
    type token_filter =
      (t * Loc.t) Stream.t -> (t * Loc.t) Stream.t

    type t = unit

    let mk _ = ()
    let filter _ strm = strm
    let define_filter _ _ = ()
    let keyword_added _ _ _ = ()
    let keyword_removed _ _ = ()
  end
{% endhighlight %}

<b>Lexer</b>

Finally we have `Lexer`, which packages up the other modules and
provides the actual lexing function. The lexing function takes an
initial location and a character stream, and returns a stream of token
and location pairs:

{% highlight ocaml %}
module type Lexer = sig
  module Loc : Loc
  module Token : Token with module Loc = Loc
  module Error : Error

  val mk :
    unit ->
    (Loc.t -> char Stream.t -> (Token.t * Loc.t) Stream.t)
end
{% endhighlight %}

I don't want to go through the whole lexing function; it is not very
interesting. But here is the main loop:

{% highlight ocaml %}
let rec token c = lexer
  | eof -> EOI

  | newline -> next_line c; token c c.lexbuf
  | blank+ -> token c c.lexbuf

  | '-'? ['0'-'9']+ ('.' ['0'-'9']* )?
      (('e'|'E')('+'|'-')?(['0'-'9']+))? ->
        NUMBER (L.utf8_lexeme c.lexbuf)

  | [ "{}[]:," ] | "null" | "true" | "false" ->
      KEYWORD (L.utf8_lexeme c.lexbuf)

  | '"' ->
      set_start_loc c;
      string c c.lexbuf;
      STRING (get_stored_string c)

  | "$" ->
      set_start_loc c;
      c.enc := Ulexing.Latin1;
      let aq = antiquot c lexbuf in
      c.enc := Ulexing.Utf8;
      aq

  | _ -> illegal c
{% endhighlight %}

The `lexer` syntax is an extension provided by `ulex`; the effect is
similar to `ocamllex`. The lexer needs to keep track of the current
location and return it along with the token (`next_line` advances the
current location; `set_start_loc` is for when a token spans multiple
`ulex` lexemes). The lexer also needs to parse antiquotations, taking
into account nested quotations within them.

(I think it is not actually necessary to lex JSON as UTF8. The only
place that non-ASCII characters can appear is in a string. To lex a
string we just accumulate characters until we see a double-quote,
which cannot appear as part of a multibyte character. So it would work
just as well to accumulate bytes. I am no Unicode expert though. This
example was extracted from the Javascript parser in
[jslib](http://github.com/jaked/ocamljs/tree/master/src/jslib/), where
I think UTF8 must be taken into account.)

<b>Hooking up the lexer</b>

There are a handful of changes we need to make to call the custom lexer:

In `Jq_parser` we make the grammar with the custom lexer module, and
open it so the token constructors are available; we also replace the
`INT` and `FLOAT` cases with just `NUMBER`; for the other cases we
used the same token constructor names as the default lexer so we don't
need to change anything.

{% highlight ocaml %}
  open Jq_lexer

  module Gram = Camlp4.PreCast.MakeGram(Jq_lexer)

  ...
      | n = NUMBER -> Jq_number (float_of_string n)
{% endhighlight %}

In `Jq_quotations` we have `Camlp4.PreCast` open (so references to
`Ast` in the `<:expr< >>` quotations resolve), so `EOI` is
`Camlp4.PreCast.EOI`; we want `Jq_lexer.EOI`, so we need to write it
explicitly:

{% highlight ocaml %}
  json_eoi: [[ x = Jq_parser.json; `Jq_lexer.EOI -> x ]];
{% endhighlight %}

(Recall that the backtick lets us match a constructor directly; for
some reason we can't module-qualify `EOI` without it.)

That's it.

I want to finish off this series next time by covering grammar
extension, with an example OCaml syntax extension.

(You can find the complete code for this example
[here](http://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/camlp4-custom-lexers).)
