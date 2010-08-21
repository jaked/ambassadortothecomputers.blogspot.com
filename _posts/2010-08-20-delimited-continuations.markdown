---
layout: post
title: Mixing monadic and direct-style code with delimited continuations
---

The [Lwt](http://ocsigen.org/lwt) library is a really nice way to
write concurrent programs. A big downside, however, is that you can't
use direct-style libraries with it. Suppose we're writing an XMPP
server, and we want to parse XML as it arrives over a network
connection, using Daniel Bünzli's nice
[`xmlm`](http://erratique.ch/software/xmlm) library. `Xmlm` can read
from a `string`, or from a `Pervasives.in_channel`, or you can give it
a function of type `(unit -> int)` to return the next character of
input. But there is no way to have it read from an Lwt thread; that
is, we can't give it a function of type `(unit -> int Lwt.t)`, since
it doesn't know what do with an `Lwt.t`. To keep track of the parser
state at the point the input blocks, the whole library would need to
be rewritten in Lwt style (i.e. monadic style).

Now, Lwt does provide the `Lwt_preemptive` module, which gives you a
way to spin off a preemptive thread (implemented as an ordinary OCaml
thread) and wait for its result in the usual Lwt way with `bind`. This
is useful, but has two drawbacks: preemptive threads are _preemptive_,
so you're back to traditional locking if you want to operate on shared
data; and preemptive threads are _threads_, so they are much heavier
than Lwt threads, and (continuing the XMPP hypothetical) it may not be
feasible to use one per open connection.

<b>Fibers</b>

What we would really like is to be able spin off a cooperative,
direct-style thread. The thread needs a way to block on Lwt threads,
but when it blocks we need to be able to schedule another Lwt
thread. As a cooperative thread it of course has exclusive access to
the process state while it is running. A cooperative, direct-style
thread is sometimes called a _coroutine_ (although to me that word
connotes a particular style of inter-thread communication as well,
where values are yielded between coroutines), or a _fiber_.

Here's an API for mixing Lwt threads with fibers:

{% highlight ocaml %}
  val start : (unit -> 'a) -> 'a Lwt.t
  val await : 'a Lwt.t -> 'a
{% endhighlight %}

The `start` function spins off a fiber, returning an Lwt thread which
is woken with the result of the fiber once it completes. The `await`
function (which may be called only from within a fiber) blocks on the
result of an Lwt thread, allowing another Lwt thread to be scheduled
while it is waiting.

With this API we could implement our XMPP server by calling `xmlm`
from within a fiber, and passing it a function that `await`s the next
character available on the network connection. But how do we implement
it?

<b>Delimited continuations</b>

[Oleg Kiselyov](http://okmij.org/ftp/)'s recent
[announcement](http://caml.inria.fr/pub/ml-archives/caml-list/2010/08/3567e58838e79cacc3441da7508d46fe.en.html)
of a native-code version of his `Delimcc` library for delimited
continuations in OCaml reminded me of two things:

 1. I should find out what delimited continuations are.
 2. They sound useful for implementing fibers.

The paper describing the library,
[Delimited Control in OCaml, Abstractly and Concretely](http://okmij.org/ftp/continuations/caml-shift.pdf),
has a pretty good overview of delimited continuations. The core API is small:

{% highlight ocaml %}
  type 'a prompt
  type ('a,'b) subcont
  
  val new_prompt   : unit -> 'a prompt
  
  val push_prompt  : 'a prompt -> (unit -> 'a) -> 'a
  val take_subcont :
    'b prompt -> (('a,'b) subcont -> unit -> 'b) -> 'a
  val push_subcont : ('a,'b) subcont -> (unit -> 'a) -> 'b
{% endhighlight %}

Paraphrasing the paper: A prompt is like an exception. The call
`push_prompt p f` is like a `try`/`with` expression, and
`take_subcont p g` is like `raise`, except that rather than
pattern-matching the exception, we handle only the exact prompt `p`,
and instead of running some arbitrary code when we handle it, we run
`g`.

The big difference is that `g` is passed a handle `sk` to the control
point at which the exception was raised (i.e. `take_subcont` was
called). We can resume control at that point with `push_subcont sk h`,
and instead of raising the exception, the result of `h` is returned.

A somewhat lower-level view of things is that `push_prompt p f` marks
the stack with `p` then runs `f`; `take_subcont p g` unwinds the stack
back to `p`, then passes the unwound fragment to `g`; and
`push_subcont sk h` restores the stack fragment `sk` then runs
`h`. (Based on a pretty cursory reading of the paper it sounds like
this is how the library is implemented.)

<b>Implementing fibers</b>

To implement fibers, we want `start f` to mark the stack, then run
`f`; and `await t` to unwind the stack, wait for `t` to complete, then
restore the stack. Here is `start`:

{% highlight ocaml %}
  let active_prompt = ref None
  
  let start f =
    let t, u = Lwt.wait () in
    let p = Delimcc.new_prompt () in
    active_prompt := Some p;
  
    Delimcc.push_prompt p begin fun () ->
      let r =
        try Lwt.Return (f ())
        with e -> Lwt.Fail e in
      active_prompt := None;
      match r with
        | Lwt.Return v -> Lwt.wakeup u v
        | Lwt.Fail e -> Lwt.wakeup_exn u e
        | Lwt.Sleep -> assert false
    end;
    t
{% endhighlight %}

We make a sleeping Lwt thread, and store a new prompt in a global
(this is OK because we won't yield control to another Lwt thread
before using it; of course this is not safe with OCaml threads). Then
we mark the stack with `push_prompt` and run the fiber. The
`let r = ... match r with ...` is to avoid calling `Lwt.wakeup{,_exn}`
in the scope of the `try`; we use `Lwt.state` as a handy type to store
either a result or an exception. If the fiber completes without
calling `await` then all we do is wake up the Lwt thread with the
returned value or exception.

Here is a first try at `await`:

{% highlight ocaml %}
  let await t =
    let p =
      match !active_prompt with
        | None -> failwith "await called outside start"
        | Some p -> p in
    active_prompt := None;

    match Lwt.poll t with
      | Some v -> v
      | None ->
          Delimcc.take_subcont p begin fun sk () ->
            let ready _ =
              active_prompt := Some p;
              Delimcc.push_subcont sk begin fun () ->
                match Lwt.poll t with
                  | Some v -> v
                  | None -> assert false
              end;
              Lwt.return () in
            ignore (Lwt.try_bind (fun () -> t) ready ready)
          end
{% endhighlight %}

We first check to be sure that we are in the scope of `start`, and
that `t` isn't already completed (in which case we can just return its
result). Then we call `take_subcont` to unwind the stack, and
`try_bind` so we can restore the stack fragment when `t` completes
(whether by success or failure). The `ready` function restores the
global `active_prompt`, in case the fiber calls `await` again, then
restores the stack with `push_subcont`, and finally returns the value
(or exception) of `t`. We need `Lwt.return ()` to make the types work
out, but what happens to the rest of the computation after we call
`push_subcont`? We are somewhere in `Lwt.restart` at this point and
need to run other waiters. Hmm...

This version works for the first call to `await`, but if you make a
second call you get `Failure "No prompt was set"`. It turns out that
`push_prompt` has another difference from `try`/`with`---it only works
once. We need to call it again to reset the exception handler:

{% highlight ocaml %}
          Delimcc.take_subcont p begin fun sk () ->
            let ready _ =
              active_prompt := Some p;
              Delimcc.push_prompt p begin fun () ->
                Delimcc.push_subcont sk begin fun () ->
                  match Lwt.poll t with
                    | Some v -> v
                    | None -> assert false
                end
              end;
              Lwt.return () in
            ignore (Lwt.try_bind (fun () -> t) ready ready)
          end
{% endhighlight %}

When `t` completes, we mark the stack again, then restore the saved
stack fragment to continue the fiber. The next time the fiber calls
`await`, control returns to the point after `push_prompt`, and we
continue running the waiters of `t`. We have been carrying around the
continuation of the original call to the fiber (`f ()` in `start`) in
the saved stack fragments, so when `f` finally returns we wake up the
Lwt thread we created in `start`. (I'll be honest, I don't 100%
understand this.)

Rooting around in `delimcc.ml` we find another function,
`push_delim_subcont`, which is supposed to be an optimized version of
the `push_prompt`/`push_subcont` pair. And we find `shift0`, which
looks a lot like the `take_subcont`/`push_delim_subcont` pattern. Can
we use it?

{% highlight ocaml %}
          Delimcc.shift0 p begin fun k ->
            let ready _ =
              active_prompt := Some p;
              begin match Lwt.poll t with
                | Some v -> k v
                | None -> assert false
              end;
              Lwt.return () in
            ignore (Lwt.try_bind (fun () -> t) ready ready)
          end
{% endhighlight %}

It turns out no. This works fine if `t` succeeds, but if it fails, the
call to `Lwt.poll` raises the exception at the wrong place---we
haven't yet restored the stack. If `shift0` took a thunk rather than a
value, we could use it. So here is our final version:

{% highlight ocaml %}
          Delimcc.take_subcont p begin fun sk () ->
            let ready _ =
              active_prompt := Some p;
              Delimcc.push_delim_subcont sk begin fun () ->
                match Lwt.poll t with
                  | Some v -> v
                  | None -> assert false
              end;
              Lwt.return () in
            ignore (Lwt.try_bind (fun () -> t) ready ready)
          end
{% endhighlight %}

<b>Events vs. threads</b>

We have seen that we can use fibers to write Lwt threads in direct
style. Should we abandon Lwt's monadic style entirely, and use Lwt
only for its event handling?

First, how does each style perform? Every time a fiber blocks and
resumes, we have to copy, unwind, and restore its entire stack. With
Lwt threads, the "stack" is a bunch of linked closures in the heap, so
we don't need to do anything to block or resume. On the other hand,
building and garbage-collecting the closures is more expensive than
pushing and popping the stack. We can imagine that which style
performs better depends on the thread: if it blocks infrequently
enough, the amortized cost of copying and restoring the stack might be
lower than the cost of building and garbage-collecting the
closures. (We can also imagine that a different implementation of
delimited continuations might change this tradeoff.)

Second, how does the code look? The paper
[Cooperative Task Management without Manual Stack Management](http://www.stanford.edu/class/cs240/readings/usenix2002-fibers.pdf)
considers this question in the context of the "events vs. threads"
debate. Many of its points lose their force when translated to OCaml
and Lwt---closures, the `>>=` operator, and Lwt's syntax extension go
a long way toward making Lwt code look like direct style---but some
are still germane. In favor of fibers is that existing direct-style
code need not be rewritten to work with Lwt (what motivated us in the
first place). In favor of monadic style is that the type of a function
reflects the possibility that it might block, yield control to
another thread, and disturb state invariants.

<b>Direct-style FRP</b>

We could apply this idea, of replacing monadic style with direct style
using delimited continuations, to other monads---in particular to the
[`froc`](http://github.com/jaked/froc) library for functional reactive
programming. (The Scala.React FRP library also uses delimited
continuations to implement direct style; see
[Deprecating the Observer Pattern](http://lamp.epfl.ch/~imaier/pub/DeprecatingObserversTR2010.pdf)
for details.)

Here's the API:

{% highlight ocaml %}
  val direct : (unit -> 'a) -> 'a Froc.behavior
  val read : 'a Froc.behavior -> 'a
{% endhighlight %}

Not surprisingly, it's just the same as for Lwt, but with a different
monad and different names (I don't know if `direct` is quite right but
it is better than `start`). There is already a function `Froc.sample`
with the same type as `read`, but it has a different meaning: `sample`
takes a snapshot of a behavior but creates no dependency on it.

The implementation is very similar as well:

{% highlight ocaml %}
  let active_prompt = ref None
  
  let direct f =
    let t, u = Froc_ddg.make_changeable () in
    let p = Delimcc.new_prompt () in
    active_prompt := Some p;
  
    Delimcc.push_prompt p begin fun () ->
      let r =
        try Froc_ddg.Value (f ())
        with e -> Froc_ddg.Fail e in
      active_prompt := None;
      Froc_ddg.write_result u r
    end;
    (Obj.magic t : _ Froc.behavior)
{% endhighlight %}

This is essentially the same code as `start`, modulo the change of
monad. However, some of the functions we need aren't exported from
`Froc`, so we need to use the underlying `Froc_ddg` module and magic
the result at the end. `Froc_ddg.make_changeable` is the equivalent of
`Lwt.wait`: it returns an "uninitialized" monadic value along with a
writer for that value. We use `Froc_ddg.result` instead of `Lwt.state`
to store a value or exception, and `Froc_ddg.write_result` instead of
the pattern match and `Lwt.wakeup{,_exn}`.

{% highlight ocaml %}  
  let read t =
    let p =
      match !active_prompt with
        | None -> failwith "read called outside direct"
        | Some p -> p in
    active_prompt := None;
  
    Delimcc.take_subcont p begin fun sk () ->
      Froc.notify_result_b t begin fun r ->
        active_prompt := Some p;
        Delimcc.push_delim_subcont sk begin fun () ->
          match r with
            | Froc.Value v -> v
            | Froc.Fail e -> raise e
        end
      end
    end
{% endhighlight %}

And this is essentially the same code as `await`. A `Froc.behavior`
always has a value, so we don't poll it as we did with `Lwt.t`, but go
straight to `take_subcont`. We have `Froc.try_bind` but it's a little
more compact to use use `notify_result_b`, which passes a `result`.

<b>Monadic reflection</b>

The similarity between these implementations suggests that we could
use the same code to get a direct style version of any monad; we only
need a way to create an uninitialized monadic value, then set it. The
call to `Lwt.poll` in `await` is an optimization which we would have
to forgo.  (In both these examples we have a monad with failure, and
`try_bind`, but we could do without it.)

A little googling turns up Andrzej Filinski's paper
[Representing Monads](http://www.diku.dk/hjemmesider/ansatte/andrzej/papers/RM-abstract.html),
which reaches the same conclusion, with a lot more rigor. In that work
`start`/`direct` are called `reify`, and `await`/`read` are called
`reflect`. `Reflect` seems to be close to the implementations above,
but in `reify` the paper uses the monadic `bind` and `return` rather
than creating an uninitialized monadic value and later setting it. The
delimited continuation operators are a little different; I'm not sure
how to achieve the same thing here.

(You can find the complete code for Lwt fibers
[here](http://github.com/jaked/lwt-equeue/tree/master/src/lwt-fiber) and
direct-style `froc`
[here](http://github.com/jaked/froc/tree/master/src/froc-direct).)
