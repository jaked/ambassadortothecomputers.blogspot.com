---
layout: post
title: How froc works
---

I am happy to announce the release of version 0.2 of the `froc`
library for functional reactive programming in OCaml. There are a
number of improvements:

 * better event model: there is now a notion of simultaneous events,
   and behaviors and events can now be freely mixed
 * [self-adjusting computation](http://ttic.uchicago.edu/~umut/projects/self-adjusting-computation/)
   is now supported via memo functions; needless recomputation can be
   avoided in some cases
 * faster priority queue and timeline data structures
 * behavior and event types split into co- and contra-variant views
   for subtyping
 * bug fixes and cleanup

Development of `froc` has moved from Google Code to Github; see

 * [project page](http://github.com/jaked/froc)
 * [documentation](http://jaked.github.com/froc)
 * [downloads](http://github.com/jaked/froc/downloads)

Thanks to Ruy Ley-Wild for helpful discussion, and to Daniel Bünzli
for helpful discussion and many good ideas in React.

I thought I would take this opportunity to explain how `froc` works,
because it is interesting, and to help putative `froc` users use it
effectively.

<b>Dependency graphs</b>

The main idea behind `froc` (and self-adjusting computation) is that
we can think of an expression as implying a dependency graph, where
each subexpression depends on its subexpressions, and ultimately on
some input values. When the input values change, we can recompute the
expression incrementally by recursively pushing changes to dependent
subexpressions.

To be concrete, suppose we have this expression:

{% highlight ocaml %}
  let u = v / w + x * y + z
{% endhighlight %}

Here is a dependency graph relating expressions to their
subexpressions:

![](http://4.bp.blogspot.com/_-BRxxZyoKFE/S-RS60NO3DI/AAAAAAAAAOw/KkXzrR_I14g/s1600/how-froc-works-a.png)

The edges aren't directed, because we can think of dependencies as
either demand-driven (to compute A, we need B), or change-driven (when
B changes, we must recompute A).

Now suppose we do an initial evaluation of the expression with `v =
4`, `w = 2`, `x = 2`, `y = 3`, and `z = 1`. Then we have (giving
labels to unlabelled nodes, and coloring the current value of each
node green):

![](http://1.bp.blogspot.com/_-BRxxZyoKFE/S-RThVz19aI/AAAAAAAAAO4/3Tpx6UqcFYQ/s1600/how-froc-works-b.png)

If we set `z = 2`, we need only update `u` to `10`, since no other
node depends on `z`. If we then set `v = 6`, we need to update `n0` to
`3`, `n2` to `9` (since `n2` depends on `n0`), and `u` to `11`, but we
don't need to update `n1`. (This is the change-driven point of view.)

What if we set `z = 2` and `v = 6` simultaneously, then do the
updates? We have to be careful to do them in the right order. If we
updated `u` first (since it depends on `z`), we'd use a stale value
for `n2`. We could require that we don't update an expression until
each of its dependencies has been updated (if necessary). Or we could
respect the original evaluation order of the expressions, and say that
we won't update an expression until each expression that came before
it has been updated.

In `froc` we take the second approach. Each expression is given a
_timestamp_ (not a wall-clock time, but an abstract ordered value)
when it's initially evaluated, and we re-evaluate the computation by
running through a priority queue of stale expressions, ordered by
timestamp. Here is the situation, with changed values in magenta,
stale values in red, and timestamps in gray:

![](http://1.bp.blogspot.com/_-BRxxZyoKFE/S-RUC9vfS7I/AAAAAAAAAPA/ya4vwgVjV04/s1600/how-froc-works-c.png)

If we update the stale nodes from their dependencies in timestamp
order, we get the right answer. We will see how this approach gives us
a way to handle _control dependencies_, where A does not depend on B,
but A's execution is controlled by B.

<b>Library interface</b>

The core of `froc` has the following (simplified) signature:

{% highlight ocaml %}
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
{% endhighlight %}

The type `'a t` represents _changeable values_ (or just _changeables_)
of type `'a`; these are the nodes of the dependency graph. `Return`
converts a regular value to a changeable value. `Bind` makes a new
changeable as a dependent of an existing one; the function argument is
the expression that computes the value from its dependency. We have
`>>=` as an infix synonym for `bind`; there are also multi-argument
versions (`bind2`, `bind3`, etc.) so a value can depend on more than
one other value.

We could translate the expression from the previous section as:

{% highlight ocaml %}
  let n0 = bind2 v w (fun v w -> return (v / w))
  let n1 = bind2 x y (fun x y -> return (x * y))
  let n2 = bind2 n0 n1 (fun n0 n1 -> return (n0 + n1))
  let u = bind2 n2 z (fun n2 z -> return (n2 + z))
{% endhighlight %}

There are some convenience functions in `froc` to make this more
readable (these versions are also more efficient):

{% highlight ocaml %}
  val blift : 'a t -> ('a -> 'b) -> 'b t
  val lift : ('a -> 'b) -> 'a t -> 'b t
{% endhighlight %}

`Blift` is like `bind` except that you don't need the `return` at the
end of the expression (below we'll see cases where you actually need
`bind`); `lift` is the same as `blift` but with the arguments swapped
for partial application. So we could say

{% highlight ocaml %}
  let n0 = blift2 v w (fun v w -> v / w)
  let n1 = blift2 x y (fun x y -> x * y)
  let n2 = blift2 n0 n1 (fun n0 n1 -> n0 + n1)
  let u = blift2 n2 z (fun n2 z -> n2 + z)
{% endhighlight %}

or even

{% highlight ocaml %}
  let (/) = lift2 (/)
  let ( * ) = lift2 ( * )
  let (+) = lift2 (+)
  let u = v / w + x * y + z
{% endhighlight %}

Now, there is no reason to break down expressions all the way---a node
can have a more complicated expression, for example:

{% highlight ocaml %}
  let n0 = blift2 v w (fun v w -> v / w)
  let n2 = blift3 n0 x y (fun n0 x y -> n0 + x * y)
  let u = blift2 n2 z (fun n2 z -> n2 + z)
{% endhighlight %}

There is time overhead in propagating dependencies, and space overhead
in storing the dependency graph, so it's useful to be able to control
the granularity of recomputation by trading off computation over
changeable values with computation over ordinary values.

<b>Dynamic dependency graphs</b>

Take this expression:

{% highlight ocaml %}
  let b = x = 0
  let y = if b then 0 else 100 / x
{% endhighlight %}

Here it is in `froc` form:

{% highlight ocaml %}
  let b = x >>= fun x -> return (x = 0)
  let n0 = x >>= fun x -> return (100 / x)
  let y = bind2 b n0 (fun b n0 -> if b then return 0 else n0)
{% endhighlight %}

and its dependency graph, with timestamps:

![](http://3.bp.blogspot.com/_-BRxxZyoKFE/S-RUj5r9i7I/AAAAAAAAAPI/ROpJD6sK_PI/s1600/how-froc-works-d.png)

(We begin to see why `bind` is sometimes necessary instead of
`blift`---in order to return `n0` in the `else` branch, the function
must return `'b t` rather than `'b`.)

Suppose we have an initial evaluation with `x = 10`, and we then set
`x = 0`. If we blindly update `n0`, we get a `Division_by_zero`
exception, although we get no such exception from the original
code. Somehow we need to take into account the control dependency
between `b` and `100 / x`, and compute `100 / x` only when `b` is
false. This can be accomplished by putting it inside the `else`
branch:

{% highlight ocaml %}
  let b = x >>= fun x -> return (x = 0)
  let y = b >>= fun b -> if b then return 0
                              else x >>= fun x -> return (100 / x)
{% endhighlight %}

How does this work? `Froc` keeps track of the start and finish
timestamps when running an expression, and associates dependencies
with the timestamp when they are attacheed. When an expression is
re-run, we detach all the dependencies between the start and finish
timestamps. In this case, when `b` changes, we detach the dependent
expression that divides by 0 before trying to run it.

Let's walk through the initial run with `x = 10`: Here is the graph
showing the timestamp ranges, and on the dependency edges, the
timestamp when the dependency was attached:

![](http://3.bp.blogspot.com/_-BRxxZyoKFE/S-RUxKV8mRI/AAAAAAAAAPQ/VuJ4wIzRhsg/s1600/how-froc-works-e.png)

First we evaluate `b` (attaching it as a dependent of `x` at time `0`)
to get `false`. Then we evaluate `y` (attaching it as a dependent of
`b` at time `3`): we check `b` and evaluate `n0` to get `10`
(attaching it as a dependent of `x` at time `5`). Notice that we have
a dependency edge from `y` to `n0`. This is not a true dependency,
since we don't recompute `y` when `n0` changes; rather the value of
`y` is a proxy for `n0`, so when `n0` changes we just forward the new
value to `y`.

What happens if we set `x = 20`? Both `b` and `n0` are stale since
they depend on `x`. We re-run expressions in order of their start
timestamp, so we run `b` and get `false`. Since the value of `b` has
not changed, `y` is not stale.  Then we re-run `n0`, so its value (and
the value of `y` by proxy) becomes `5`.

What happens if we set `x = 0`? We run `b` and get `true`. Now `y` is
also stale, and it is next in timestamp order. We first detach all the
dependencies in the timestamp range `4`-`9` from the previous run of
`y`: the dependency of `n0` on `x` and the proxy dependency of `y` on
`n0`. This time we take the `then` branch, so we get `0` without
attaching any new dependencies. We are done; no `Division_by_zero`
exception.

Now we can see why it's important to handle updates in timestamp
order: the value which decides a control flow point (e.g. the test of
an `if`) is always evaluated before the control branches (the `then`
and `else` branches), so we have the chance to fix up the dependency
graph before the branches are updated.

<b>Garbage collection and cleanup functions</b>

A node points to its dependencies (so it can read their values when
computing its value), and its dependencies point back to the node (so
they can mark it stale when they change). This creates a problem for
garbage collection: a node which becomes garbage (from the point of
view of the library user) is still attached to its dependencies,
taking up memory, and causing extra recomputation.

The implementation of dynamic dependency graphs helps with this
problem: as we have seen, when an expression is re-run, the
dependencies attached in the course of the previous run are detached,
including any dependencies for nodes which have become garbage. Still,
until the expression that created them is re-run, garbage nodes remain
attached.

Some other FRP implementations use weak pointers to store a node's
dependents, to avoid hanging on to garbage nodes. Since `froc` is
designed to work in browsers (using
[ocamljs](http://jaked.github.com/ocamljs)), weak pointers aren't an
option because they aren't supported in Javascript. But even in
regular OCaml, there are reasons to eschew the use of weak pointers:

First, it's useful to be able to set up changeable expressions which
are used for their effect (say, updating the GUI) rather than their
value; to do this with a system using weak pointers, you have to stash
the expression somewhere so it won't be GC'd. This is similar to the
problem of GCing threads; it doesn't make sense if the threads can
have an effect.

Second, there are other resources which may need to be cleaned up in
reaction to changes (say, GUI event handler registrations); weak
pointers are no help here. `Froc` gives you a way to set cleanup
functions during a computation, which are run when the computation is
re-run, so you can clean up other resources.

With `froc` there are two options to be sure you don't leak memory:
you can call `init` to clean up the entire system, or you can use
`bind` to control the lifetime of changeables: for instance, you could
have a changeable `c` representing a counter, do a computation in the
scope of a bind of `c` (you can just ignore the value), then increment
the counter to clear out the previous computation.

In fact, there are situations where `froc` cleans up too
quickly---when you want to hang on to a changeable after the
expression that attached it is re-run. We'll see shortly how to avoid
this.

<b>Memoizing the previous run</b>

Here is the `List.map` function, translated to work over lists
where the tail is changeable.

{% highlight ocaml %}
  type 'a lst = Nil | Cons of 'a lst t

  let rec map f lst =
    lst >>= function
      | Nil -> return Nil
      | Cons (h, t) ->
          let t = map f t in
          return (Cons (h, t))
{% endhighlight %}

What happens if we run

{% highlight ocaml %}
  map (fun x -> x + 1) [ 1; 2; 3 ]
{% endhighlight %}

? (I'm abusing the list syntax here to mean a changeable list with
these elements.) Let's see if we can fit the dependency graph on the
page (abbreviating `Cons` and `Nil` and writing just `f` for the
`function` expression):

![](http://2.bp.blogspot.com/_-BRxxZyoKFE/S-RVCDRbyuI/AAAAAAAAAPY/XKaamcWm3QE/s1600/how-froc-works-f.png)

(The dependency edges on the right-hand side don't mean that e.g. `f0`
depends directly on `f1`, but rather that the value returned by
`f0`---`Cons(2,f1)`---depends on `f1`. We don't re-run `f0` when `f1`
changes, or even update its value by proxy as we did in the previous
section. But if `f1` is stale it must be updated before we can
consider `f0` up-to-date.)

Notice how the timestamp ranges for the `function` expressions are
nested each within the previous one. There is a control dependency at
each recursive call: whether we make a deeper call depends on whether
the argument list is `Nil`.

So if we change `t3`, just `f3` is stale. But if we change `t0`, we
must re-run `f0`, `f1`, `f2`, and `f3`---that is, the whole
computation---detaching all the dependencies, then reattaching
them. This is kind of annoying; we do a lot of pointless work since
nothing after the first element has changed.

If only some prefix of the list has changed, we'd like to be able to
reuse the work we did in the previous run for the unchanged
suffix. `Froc` addresses this need with _memo functions_. In a way
similar to ordinary memoization, a memo function records a table of
arguments and values when you call it. But in `froc` we only reuse
values from the previous run, and only those from the timestamp range
we're re-running. We can define `map` as a memo function:

{% highlight ocaml %}
  let map f lst =
    let memo = memo () in
    let rec map lst =
      lst >>= function
        | Nil -> return Nil
        | Cons (h, t) ->
            let t = memo map t in
            return (Cons (h, t)) in
    memo map lst
{% endhighlight %}

Here the `memo` call makes a new memo table. In the initial run we add
a memo entry associating each list node (`t0`, `t1`, ...) with its
`map` (`f0`, `f1`, ...). Now, suppose we change `t0`: `f0` is stale,
so we update it. When we go to compute `map f t1` we get a memo hit
returning `f1` (the computation of `f1` is contained in the timestamp
range of `f0`, so it is a candidate for memo matching). `F1` is
up-to-date so we return it as the value of `map f t1`.

There is a further wrinkle: suppose we change both `t0` and `t2`,
leaving `t1` unchanged. As before, we get a memo hit on `t1` returning
`f1`, but since `f2` is stale, so is `f1`. We must run the update
queue until `f1` is up-to-date before we return it as the value of
`map f t1`. Recall that we detach the dependencies of the computation
we're re-running; in order to update `f1` we just leave it attached to
its dependencies and run the queue until the end of its timestamp
range.

In general, there can be a complicated pattern of changed and
unchanged data---we could change every other element in the list, for
instance---so memoization and the update loop call one another
recursively. From the timestamp point of view, however, we can think
of it as a linear scan through time, alternating between updating
stale computations and reusing ones which have not changed.

The memo function mechanism provides a way to keep changeables
attached even after the expression that attached them is re-run. We
just need to attach them from within a memo function, then look them
up again on the next run, so they're left attached to their
dependencies. The
[quickhull](http://jaked.github.com/froc/examples/froc-dom/quickhull)
example
([source](http://jaked.github.com/froc/examples/froc-dom/quickhull/quickhull.ml))
demonstrates how this works.

<b>Functional reactive programming and the event queue</b>

Functional reactive programming works with two related types:
_behavior_s are values that can change over time, but are defined at
all times; _event_s are defined only at particular instants in time,
possibly (but not necessarily) with a different value at each
instant. (_Signal_s are events or behaviors when we don't care which
one.)

Events can be used to represent external events entering the system
(like GUI clicks or keystrokes), and can also represent occurrences
within the system, such as a collision between two moving objects. It
is natural for events to be defined in terms of behaviors and vice
versa. (In fact they can be directly interdefined with the `hold` and
`changes` functions.)

In `froc`, behaviors are just another name for changeables. Events are
implemented on top of changeables: they are just changeables with
transient values. An incoming event sets the value of its underlying
changeable; after changes have propagated through the dependency
graph, the values of all the changeables which underlie events are
removed (so they can be garbage collected).

Signals may be defined (mutually) recursively. For example, in
the [bounce](http://jaked.github.com/froc/examples/froc-dom/bounce)
example
([source](http://jaked.github.com/froc/examples/froc-dom/bounce/bounce.ml)),
the position of the ball is a behavior defined in terms of its
velocity, which is a behavior defined in terms of events indicating
collisions with the walls and paddle, which are defined in terms of
the ball's position.

`Froc` provides the `fix_b` and `fix_e` functions to define signals
recursively. The definition of a signal can't refer directly to its
own current value, since it hasn't been determined yet; instead it
sees its value from the previous update cycle. When a
recursively-defined signal produces a value, an event is queued to be
processed in the next update cycle, so the signal can be updated based
on its new current value. (If the signal doesn't converge somehow this
process loops.)

<b>Related systems</b>

`Froc` is closely related to a few other FRP systems which are
change-driven and written in an imperative, call-by-value language:

[FrTime](http://www.cs.brown.edu/~greg/) is an FRP system for PLT
Scheme. FrTime has a dependency graph and update queue mechanism
similar to `froc`, but sorts stale nodes in dependency ("topological")
rather than timestamp order. There is a separate mechanism for
handling control dependencies, using a dynamic scoping feature
specific to PLT Scheme ("parameters") to track dependencies attached
in the course of evaluating an expression; in addition FrTime uses
weak pointers to collect garbage nodes. There is no equivalent of
`froc`'s memo functions. Reactivity in FrTime is implicit: you give an
ordinary Scheme program, and the compiler turns each subexpression
into a changeable value. There is no programmer control over the
granularity of recomputation, but there is a compiler optimization
("lowering") which recovers some performance by coalescing
changeables.

[Flapjax](http://www.flapjax-lang.org/) is a descendent of FrTime for
Javascript. It implements the same dependency-ordered queue as FrTime,
but there is no mechanism for control dependencies, and there are no
weak pointers (since there are none in Javascript), so it is fairly
easy to create memory leaks (although there is a special
reference-counting mechanism in certain cases). Flapjax can be used as
a library; it also has a compiler similar to FrTime's, but since it
doesn't handle control dependencies, the semantics of compiled
programs are not preserved (e.g. you can observe exceptions that don't
occur in the original program).

[React](http://erratique.ch/software/react) is a library for OCaml,
also based on a dependency-ordered queue, using weak pointers, without
a mechanism for control dependencies.

<b>Colophon</b>

I used [Mlpost](http://mlpost.lri.fr/) to generate the dependency
graph diagrams. It is very nice!
