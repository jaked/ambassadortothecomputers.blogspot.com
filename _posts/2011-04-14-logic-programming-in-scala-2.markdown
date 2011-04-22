---
layout: blogspot
title: Logic programming in Scala part 2, backtracking and state
---

In the [previous post](/2011/04/logic-programming-in-scala-part-1.html)
we saw how to write computations in a logic monad, where a "value" is
a choice among alternatives, and using a value in a computation means
using the alternatives, in some sense.

Our first implementation of the logic monad represents a choice among
alternatives as a list, and using a value in a computation means
running the computation for each alternative. This is OK for some
problems, but we run into trouble when there are a large (or infinite)
number of alternatives. For example, a choice among the natural numbers:

{% highlight scala %}
scala> import LogicList._
import LogicList._

scala> val nat: T[Int] = unit(0) | nat.map(_ + 1)
java.lang.NullPointerException
        ...
scala> def nat: T[Int] = unit(0) | nat.map(_ + 1)
nat: LogicList.T[Int]
scala> run(nat, 10)
java.lang.StackOverflowError
        ...
{% endhighlight %}

With the `val` version, Scala's implementation of recursive values
shows through: `nat` is initialized to `null` then assigned after the
right hand side is evaluated, but the evaluation fails since `nat` is
`null`. With the `def` version we can construct the value (it is a
function so the recursive use is delayed), but any attempt to use it
overflows the stack trying to compute all the natural numbers.

Let's repair this with a fancier implementation of the logic monad,
translated from Kiselyov et al.'s
[Backtracking, Interleaving, and Terminating Monad Transformers](http://okmij.org/ftp/Computation/LogicT.pdf).

<b>Success and failure continuations</b>

The idea is to represent a choice of alternatives by a function, which
takes as arguments two functions: a _success continuation_ and a
_failure continuation_. (Here a continuation is nothing fancy, just a
function indicating what to do next.)

The success continuation is called with each alternative in the choice
until there are no more; then the failure continuation is called. (If
there are no alternatives of course the failure continuation is called
immediately.) However, in contrast to the list implementation, the
success continuation is not called eagerly for each alternative, but
rather on demand as the alternatives are needed.

The key idea is what happens for `t1 | t2`: we pass `t1` a failure
continuation which calls the success continuation on `t2`, so the
resulting value calls the success continuation on each alternative of
both `t1` and `t2`.

Let's see the code:

{% highlight scala %}
object LogicSFK extends Logic {
  type FK[R] = () => R
  type SK[A,R] = (A, FK[R]) => R

  trait T[A] { def apply[R](sk: SK[A,R], fk: FK[R]): R }
{% endhighlight %}

The continuations can return a result of some arbitrary type `R`. This
means that the representation function has a rank-2 polymorphic
type---it takes functions which are themselves polymorphic---which is
not directly representable in Scala. But we can encode it by making
the representation function a method.

The success continuation takes a value of the underlying type, and
also a failure continuation. Whatever use the computation makes of the
value, it may eventually fail (by returning `fail` or by using a guard
in a for-comprehension which is always false); at that point the
failure continuation is called.

{% highlight scala %}
  def fail[A] =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: FK[R]) = fk()
    }

  def unit[A](a: A) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: FK[R]) = sk(a, fk)
    }
{% endhighlight %}

To fail, just call the failure continuation. To succeed with one
alternative, call the success continuation with the single alternative
and the passed-in failure continuation---there are no more
alternatives to try, so if this branch of the computation fails, do
whatever the caller tells us to do.

{% highlight scala %}
  def or[A](t1: T[A], t2: => T[A]) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: FK[R]) = t1(sk, { () => t2(sk, fk) })
    }
{% endhighlight %}

We want to explore the alternatives in both `t1` and `t2`, so we pass
the success continuation to `t1` (which calls it on each alternative);
when `t1` is exhausted we pass the success continuation to `t2`;
finally we fail with the caller's failure continuation.

Since a choice of alternatives is always built up from `or`, you can
see that the failure continuation means "go back and try the next
alternative". When there are no more alternatives in the current
choice, it means "go back to the previous choice and try the next
alternative". In the jargon of logic programming, an `or` is called a
_choice point_, and going back to a previous choice to try the next
alternative is called _backtracking_.

{% highlight scala %}
  def bind[A,B](t: T[A], f: A => T[B]) =
    new T[B] {
      def apply[R](sk: SK[B,R], fk: FK[R]) =
        t(({ (a, fk) => f(a)(sk, fk) }: SK[A,R]), fk)
    }

  def apply[A,B](t: T[A], f: A => B) =
    new T[B] {
      def apply[R](sk: SK[B,R], fk: FK[R]) =
        t(({ (a, fk) => sk(f(a), fk) }: SK[A,R]), fk)
    }
{% endhighlight %}

For `bind` we want to call `f` on each alternative, so we pass a
success continuation which calls `f` on its argument `a`. In turn
`f(a)` returns a choice of alternatives; we pass it the original
success continuation, and the failure continuation in force at the
point `a` was generated.

For `map` things are somewhat simpler, since `f(a)` returns a single
value rather than a choice of alternatives, so we succeed immediately
with the returned value.

{% highlight scala %}
  def filter[A](t: T[A], p: A => Boolean) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: FK[R]) =
        t(({ (a, fk) => if (p(a)) sk(a, fk) else fk() }: SK[A,R]), fk)
    }
{% endhighlight %}

To filter a choice of alternatives, every time the choice succeeds
with a value (that is, for each alternative) we see if the value
satisfies the predicate `p`. If it does, we succeed with that value;
otherwise we fail (to backtrack and generate the next alternative).

{% highlight scala %}
  def split[A](t: T[A]) = {
    def unsplit(fk: FK[Option[(A,T[A])]]): T[A] =
      fk() match {
        case None => fail
        case Some((a, t)) => or(unit(a), t)
      }
    def sk : SK[A,Option[(A,T[A])]] =
      { (a, fk) => Some((a, bind(unit(fk), unsplit))) }
    t(sk, { () => None })
  }
}
{% endhighlight %}

The point of `split` is to pull a single alternative from a choice,
returning along with it a choice of the remaining alternatives. For
the list implementation we just returned the head and tail of the
list. In this implementation, the alternatives are computed on demand;
we want to be careful only to do as much computation as needed to pull
the first alternative

The failure continuation we pass to `t` just returns `None` when there
are no more alternatives. The success continuation `sk` returns the
first alternative and a choice of the remaining alternatives (wrapped
in `Some`).

The tricky part is computing the choice of remaining alternatives:
We're given the failure continuation `fk`; calling it calls `sk` on
the next alternative, which ultimately returns `Some(a, t)` where `a`
is the next alternative, or `None` if there are no more
alternatives. We repackage this `Option` as a choice of alternatives
with `unsplit`. So that we don't call `fk` too soon, we call `unsplit`
via `bind`, which defers it until the resulting choice of alternatives
is actually used.

Now we can write infinite choices:

{% highlight scala %}
scala> import LogicSFK._
import LogicSFK._

scala> val nat: T[Int] = unit(0) | nat.map(_ + 1)
nat: LogicSFK.T[Int] = LogicSFK$$anon$3@27aea0c1

scala> run(nat, 10)
res1: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
{% endhighlight %}

This was a pretty complicated way to generate the natural numbers up
to 10; the point is that we can use this choice of alternatives in
computations.

Note that this is not quite the same as the similar-looking infinite
list in Haskell:

{% highlight haskell %}
nat = 0 : map (+1) nat
{% endhighlight %}

Each element in this list is computed just once and memoized, but in a
choice of alternatives no results are memoized; to compute each
successive number in `nat` all the previous ones must be
recomputed. So the running time of `run(nat, N)` is O(N<sup>2</sup>).

<b>Tail recursion and heap space</b>

The previous implementation isn't very efficient in Scala. One problem
is its use of continuation-passing style: because Scala doesn't
implement tail-call elimination, every call to a success or failure
continuation adds a frame to the stack, even though all we ever do
with the result of these calls is return it (that is, the calls are
always in tail position).

Surprisingly, however, we run out of memory before we run out of
stack:

{% highlight scala %}
scala> run(nat, 2000)
Java.lang.OutOfMemoryError: Java heap space
	...
{% endhighlight %}

A little heap profiling shows that we're using quadratic space as well
as quadratic time. It turns out that our implementation of `Logic.run`
(from the previous post) has a space leak. The call to `run` is not
tail-recursive (we cons `a` onto the returned list), so the stack
frame hangs around, and although `t` is dead after `split(t)`, there's
still a reference to it on the stack.

We can rewrite `run` with an accumulator to be tail-recursive:

{% highlight scala %}
  def run[A](t: T[A], n: Int): List[A] = {
    def runAcc(t: T[A], n: Int, acc: List[A]): List[A] =
      if (n <= 0) acc.reverse else
        split(t) match {
          case None => Nil
          case Some((a, t)) => runAcc(t, n - 1, a :: acc)
        }
    runAcc(t, n, Nil)
  }
{% endhighlight %}

Now `scalac` compiles `runAcc` as a loop, so there are no stack frames
holding on to dead values of `t`, and we get the expected:

{% highlight scala %}
scala> run(nat, 9000)
java.lang.StackOverflowError
	...
{% endhighlight %}

So Scala's local tail-call optimization is useful to reduce not just
stack space, but also heap space. (We could of course write this as an
ordinary loop instead.)

<b>Backtracking with exceptions</b>

The Kiselyov paper offers another implementation of the logic monad in
terms of delimited continuations. This isn't quite helpful; while
Scala implements delimited continuations, it does so by converting
code to continuation-passing style (see Rompf et al.'s
[Implementing First-Class Polymorphic Delimited Continuations by a Type-Directed Selective CPS-Transform](http://lamp.epfl.ch/~rompf/continuations-icfp09.pdf).
So they suffer the same problem with un-eliminated tail calls.

Still the delimited continuation implementation points toward another
implementation using exceptions instead of failure continuations. The
`prompt` and `abort` delimited control operators are similar to
`catch` and `throw`---the first marks a point on the stack, the second
unwinds the stack to that point. However, exceptions don't provide
`shift` (which packages up a stack fragment as a function) so we'll
need to hack around it.

{% highlight scala %}
object LogicSKE extends Logic {
  case object Fail extends Exception

  type T[A] = (A => Unit) => Unit
{% endhighlight %}

The type representing a choice among alternatives is now a function
taking just a success continuation; failure is represented by throwing
a `Fail` exception. We won't need to return values from the
continuations (see `run` below) so we don't need to encode a rank-2
type.

{% highlight scala %}
  def fail[A] = { sk => throw Fail }

  def unit[A](a: A) = { sk => sk(a) }
{% endhighlight %}

To fail we just throw `Fail`; to succeed with one alternative we just
call the success continuation on the value.

{% highlight scala %}
  def or[A](t1: T[A], t2: => T[A]) =
    { sk =>
      try { t1(sk) }
      catch { case Fail => t2(sk) }
    }
{% endhighlight %}

We pass the success continuation first to `t1`; when `t2` runs out of
alternatives and throws `Fail`, we catch it and pass the success
continuation to `t2`. So The success continuation is called on each
alternative in both `t1` and `t2`.

{% highlight scala %}
  def bind[A,B](t: T[A], f: A => T[B]) =
    { sk => t(a => f(a)(sk)) }

  def apply[A,B](t: T[A], f: A => B) =
    { sk => t(a => sk(f(a))) }

  def filter[A](t: T[A], p: A => Boolean) =
    { sk =>
      t(a => if (p(a)) sk(a) else throw Fail)
    }
{% endhighlight %}

`Bind` and `apply` are much the same as before, but without the
failure continuations. `Filter` is also much the same, except that we
throw `Fail` instead of calling a failure continuation when the
predicate is not satisfied.

{% highlight scala %}
  def split[A](t: T[A]) = throw new Exception("unimplemented")

  case object Finish extends Exception

  override def run[A](t: T[A], n: Int): List[A] = {
    if (n <= 0) return Nil
    val lb = new scala.collection.mutable.ListBuffer[A]
    def sk(a: A) = {
      lb += a
      throw (if (lb.size < n) Fail else Finish)
    }
    try {
      t(sk)
      throw new Exception("not reached")
    }
    catch { case Fail | Finish => lb.result }
  }
}
{% endhighlight %}

Here's where things get hacky. It's difficult to implement `split`
using exceptions (see `LogicSKE2` in the
[complete code](https://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/scala-logic)
for an attempt---it works but is very slow). Instead we implement
`run` directly: every time the success continuation is called we
append the result to a mutable list. When we have enough results we
throw `Finish` to stop generating them.

(In the paper, `split` is used to implement search strategies other
than the depth-first strategy we use here; if you want to play with
those you can use `LogicSFK`.)

Now we make better use of the stack:

{% highlight scala %}
scala> run(nat, 15000)
res0: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, ...
{% endhighlight %}

(The size at which `run` blows up varies a lot between runs, even in
the same instance of the Scala top-level. I guess this has to do with
optimiziations performed by the JVM's JIT compiler.)

<b>State</b>

{% highlight scala %}
trait LogicState { L =>
  type T[S,A]

  def run[S,A](s: S, t: T[S,A], n: Int): List[(S,A)]
  // def split[S,A](s: S, t: T[S,A]): Option[(S,A,T[S,A])]

  // as before with extra S parameter

  def get[S]: T[S,S]
  def set[S](s: S): T[S, Unit]
}
{% endhighlight %}

{% highlight scala %}
object LogicStateSKE extends LogicState {
  type T[S,A] = (S, ((S, A) => Unit)) => Unit

  def fail[S,A] = { (s, sk) => throw Fail }

  def unit[S,A](a: A) = { (s, sk) => sk(s, a) }

  def or[S,A](t1: T[S,A], t2: => T[S,A]) =
    { (s, sk) =>
      try { t1(s, sk) }
      catch { case Fail => t2(s, sk) }
    }

  def bind[S,A,B](t: T[S,A], f: A => T[S,B]) =
    { (s, sk) => t(s, { (s, a) => f(a)(s, sk) }) }

  def apply[S,A,B](t: T[S,A], f: A => B) =
    { (s, sk) => t(s, { (s, a) => sk(s, f(a)) }) }

  def filter[S,A](t: T[S,A], p: A => Boolean) =
    { (s, sk) =>
      t(s, { (s, a) => if (p(a)) sk(s, a) else throw Fail })
    }

  def get[S] = { (s, sk) => sk(s, s) }

  def set[S](s: S) = { (_s, sk) => sk(s, ()) }

  def run[S,A](s: S, t: T[S,A], n: Int): List[(S,A)] = {
    if (n <= 0) return Nil
    val lb = new scala.collection.mutable.ListBuffer[(S,A)]
    def sk(s: S, a: A) = {
      lb += ((s, a))
      throw (if (lb.size < n) Fail else Finish)
    }
    try {
      t(s, sk)
      throw Finish // for the typechecker, not reached
    }
    catch { case Fail | Finish => lb.result }
  }
}
{% endhighlight %}
