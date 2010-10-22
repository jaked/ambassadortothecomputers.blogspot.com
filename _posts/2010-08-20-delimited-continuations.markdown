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
has a pretty good overview of delimited continuations, and section 2 of
[A Monadic Framework for Delimited Continuations](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.68.9352)
is helpful too.

The core API is small:

{% highlight ocaml %}
  type 'a prompt
  type ('a,'b) subcont
  
  val new_prompt   : unit -> 'a prompt
  
  val push_prompt  : 'a prompt -> (unit -> 'a) -> 'a
  val take_subcont :
    'b prompt -> (('a,'b) subcont -> unit -> 'b) -> 'a
  val push_subcont : ('a,'b) subcont -> (unit -> 'a) -> 'b
{% endhighlight %}

I find it easiest to think about these functions as operations on the
stack. A prompt is an identifier used to mark a point on the stack
(the stack can be marked more than once with the same prompt). The
function `new_prompt` makes a new prompt which is not equal to any
other prompt.

The call `push_prompt p f` marks the stack with `p` then runs `f`, so
the stack, growing to the right, looks like

<pre>
  ABCDpEFGH
</pre>

where `ABCD` are stack frames in the continuation of the call to
`push_prompt`, and `EFGH` are frames created while running `f`. If `f`
returns normally (that is, without calling `take_subcont`) then its
return value is returned by `push_prompt`, and we are back to the
original stack `ABCD`.

If `take_subcont p g` is called while running `f`, the stack fragment
`EFGH` is packaged up as an `('a,'b) subcont` and passed to `g`. You
can think of an `('a,'b) subcont` as a function of type `'a -> 'b`,
where `'a` is the return type of the call to `take_subcont` and `'b`
is the return type of the call to `push_prompt`. `Take_subcont`
removes the fragment `pEFGH` from the stack, and there are some new
frames `IJKL` from running `g`, so we have

<pre>
  ABCDIJKL
</pre>

Now `g` can make use of the passed-in `subcont` using
`push_subcont`. (Thinking of a `subcont` as a function, `push_subcont`
is just a weird function application operator, which takes the
argument as a thunk). Then the stack becomes

<pre>
  ABCDIJKLEFGH
</pre>

Of course `g` can call the `subcont` as many times as you like.

A common pattern is to re-mark the stack with `push_prompt` before
calling `push_subcont` (so `take_subcont` may be called again). There
is an optimized version of this combination called
`push_delim_subcont`, which produces the stack

<pre>
  ABCDIJKLpEFGH
</pre>

The idea that a `subcont` is a kind of function is realized by
`shift0`, which is like `take_subcont` except that instead of passing
a `subcont` to `g` it passes an ordinary function. The passed function
just wraps a call to `push_delim_subcont`. (It is `push_delim_subcont`
rather than `push_subcont` for historical reasons I think---see the
Monadic Framework paper for a comparison of various delimited
continuation primitives.)

<b>Implementing fibers</b>

To implement fibers, we want `start f` to mark the stack, then run
`f`; and `await t` to unwind the stack back to the mark, wait for `t`
to complete, then restore the stack. Here is `start`:

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
we mark the stack with `push_prompt` and run the fiber. (The
`let r = ... match r with ...` is to avoid calling `Lwt.wakeup{,_exn}`
in the scope of the `try`; we use `Lwt.state` as a handy type to store
either a result or an exception.) If the fiber completes without
calling `await` then all we do is wake up the Lwt thread with the
returned value or exception.

Here is `await`:

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
          Delimcc.shift0 p begin fun k ->
            let ready _ =
              active_prompt := Some p;
              k ();
              Lwt.return () in
            ignore (Lwt.try_bind (fun () -> t) ready ready)
          end;
          match Lwt.poll t with
            | Some v -> v
            | None -> assert false
{% endhighlight %}

We first check to be sure that we are in the scope of `start`, and
that `t` isn't already completed (in which case we just return its
result). If we actually need to wait for `t`, we call `shift0`, which
capture the stack fragment back to the `push_prompt` call in `start`
(this continuation includes the subsequent `match Lwt.poll t` and
everything after the call to `await`), then `try_bind` so we can
restore the stack fragment when `t` completes (whether by success or
failure). When `t` completes, the `ready` function restores the global
`active_prompt`, in case the fiber calls `await` again, then restores
the stack by calling `k` (recall that this also re-marks the stack
with `p`, which is needed if the fiber calls `await` again).

It's pretty difficult to follow what's going on here, so let's try it
with stacks. After calling `start` we have

<pre>
  ABCDpEFGH
</pre>

where `ABCD` is the continuation of `push_prompt` in `start` (just the
return of `t`) and `EFGH` are frames created by the thunk passed to
`start`. Now, a call to `await` (on an uncompleted thread) calls
`shift0`, which packs up `EFGH` as `k` and unwinds the stack to
`p`. The function passed to `shift0` stores `k` in `ready` but doesn't
call it, and control returns to `start` (since the stack has been
unwound).

The program continues normally until `t` completes. Now control is in
`Lwt.run_waiters` running threads that were waiting on `t`; one of
them is our `ready` function. When it is called, the stack is
re-marked and `EFGH` is restored, so we have

<pre>
  QRSTpEFGH
</pre>

where `QRST` is wherever we happen to be in the main program, ending
in `Lwt.run_waiters`. Now, `EFGH` ends with the second call to
`match Lwt.poll` in `await`, which returns the value of `t` and
continues the thunk passed to `start`. The stack is now marked with
`p` inside `Lwt.run_waiters`, so when `await` is called again control
returns there.

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
  
    Delimcc.shift0 p begin fun k ->
      Froc.notify_result_b t begin fun _ ->
        active_prompt := Some p;
        k ()
      end
    end;
    Froc.sample t
{% endhighlight %}

And this is essentially the same code as `await`. A `Froc.behavior`
always has a value, so we don't poll it as we did with `Lwt.t`, but go
straight to `shift0`. We have `Froc.try_bind` but it's a little more
compact to use use `notify_result_b`, which passes a `result`.

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
`reflect`. `Reflect` is close to the implementations above, but in
`reify` the paper marks the stack inside a function passed to `bind`
rather than creating an uninitialized monadic value and later setting
it.

This makes sense---inside `bind` an uninitialized monadic value is
created, then set from the result of the function passed to `bind`. So
we are partially duplicating `bind` in the code above. If we mark the
stack in the right place we should be able to use `bind` directly. It
is hard to see how to make the details work out, however, since
`Lwt.bind` and `Froc.bind` each have some cases where uninitialized
values are not created.

(You can find the complete code for Lwt fibers
[here](http://github.com/jaked/lwt-equeue/tree/master/src/lwt-fiber) and
direct-style `froc`
[here](http://github.com/jaked/froc/tree/master/src/froc-direct).)

(revised 10/22)
