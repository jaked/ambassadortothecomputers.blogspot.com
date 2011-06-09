---
layout: post
title: Logic programming in Scala part 2, backtracking
---

In the [previous post](/2011/04/logic-programming-in-scala-part-1.html)
we saw how to write computations in a logic monad, where a "value" is
a choice among alternatives, and operating on a value means operating
on all the alternatives.

Our first implementation of the logic monad represents a choice among
alternatives as a list, and operating on a value means running the
operation for each alternative immediately (to produce a new list of
alternatives). If we imagine alternatives as leaves of a tree (with
`|` indicating branching), the first implementation explores the tree
breadth-first.

This is OK for some problems, but we run into trouble when there are a
large or infinite number of alternatives. For example, a choice among
the natural numbers:

{% highlight scala %}
scala> import LogicList._
import LogicList._

scala> val nat: T[Int] = unit(0) | nat.map(_ + 1)
java.lang.NullPointerException
        ...
{% endhighlight %}

This goes wrong because even though the right-hand argument to `|` is
by-name, we immediately try to use it, and fail because `nat` is not
yet defined.

{% highlight scala %}
scala> def nat: T[Int] = unit(0) | nat.map(_ + 1)
nat: LogicList.T[Int]
scala> run(nat, 10)
java.lang.StackOverflowError
        ...
{% endhighlight %}

With `def` we can successfully define `nat`, because the right-hand
side isn't evaluated until `nat` is used in the call to `run`, but we
overflow the stack trying to compute all the natural numbers.

Let's repair this with a fancier implementation of the logic monad,
translated from Kiselyov et al.'s
[Backtracking, Interleaving, and Terminating Monad Transformers](http://okmij.org/ftp/Computation/LogicT.pdf).
This implementation will explore the tree depth-first.

<b>Success and failure continuations</b>

The idea is to represent a choice of alternatives by a function, which
takes as arguments two functions: a _success continuation_ and a
_failure continuation_. The success continuation is just a function
indicating what to do next with each alternative; the failure
continuation is what to do next when there are no more alternatives.

For success, what we do next is either return the alternative (when we
have reached a leaf of the tree), or perform some operation on it
(possibly forming new branches rooted at the alternative). For
failure, what we do next is back up to the last branch point and
succeed with the next alternative. If there are no more alternatives
at the previous branch point we back up again, and so on until we can
succeed or finally run out of alternatives. In other words, we do
depth-first search on the tree, except that the tree isn't a
materialized data structure---it's created on the fly.

(In the jargon of logic programming, a branch point is called a
"choice point", and going back to an earlier choice point is called
"backtracking".)

{% highlight scala %}
object LogicSFK extends Logic {
  type FK[R] = () => R
  type SK[A,R] = (A, FK[R]) => R

  trait T[A] { def apply[R](sk: SK[A,R], fk: FK[R]): R }
{% endhighlight %}

The continuations can return a result of some arbitrary type `R`. This
means that the function representing a choice has a "rank-2"
polymorphic type---it takes functions which are themselves
polymorphic---which is not directly representable in Scala. But we can
encode it by making the representation function a method on a trait.

The success continuation takes a value of the underlying type (i.e. an
alternative), and also a failure continuation, to call in case this
branch of the tree eventually fails (by calling `fail`, or `filter`
when no alternative satisfies the predicate). The failure continuation
is also called to succeed with the next alternative after returning a
leaf (see `split`).

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
alternatives to try, so if this branch fails the `unit` fails.

{% highlight scala %}
  def or[A](t1: T[A], t2: => T[A]) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: FK[R]) =
        t1(sk, { () => t2(sk, fk) })
    }
{% endhighlight %}

`Or` creates a choice point. We want to explore the alternatives in
both `t1` and `t2`, so we pass the success continuation to `t1` (which
calls it on each alternative); when `t1` is exhausted we pass the
success continuation to `t2`; finally we fail with the caller's
failure continuation---that is, we backtrack.

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

For `bind` we extend each branch by calling `f` on the current
leaf. To succeed we call `f` on the alternative `a`. Now `f(a)`
returns a choice of alternatives, so we pass it the original success
continuation (which says what to do next with alternatives resulting
from the `bind`), and the failure continuation in force at the point
`a` was generated (which succeeds with the next available alternative
from `f(a)`).

For `apply` things are simpler, since `f(a)` returns a single value
rather than a choice of alternatives: we succeed immediately with the
returned value.

{% highlight scala %}
  def filter[A](t: T[A], p: A => Boolean) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: FK[R]) = {
        val sk2: SK[A,R] =
          { (a, fk) => if (p(a)) sk(a, fk) else fk() }
        t(sk2, fk)
      }
    }
{% endhighlight %}

To filter a choice of alternatives, each time we succeed with a value
we see if it satisfies the predicate `p`; if it does, we succeed with
that value (extending the branch), otherwise we fail (pruning the
branch).

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
returning along with it a choice of the remaining alternatives. In the
list implementation we just returned the head and tail of the list. In
this implementation, the alternatives are computed on demand; we want
to be careful to do only as much computation as needed to pull the
first alternative

The failure continuation we pass to `t` just returns `None` when there
are no more alternatives. The success continuation `sk` returns the
first alternative and a choice of the remaining alternatives (wrapped
in `Some`).

The tricky part is the choice of remaining alternatives. We're given
the failure continuation `fk`; calling it calls `sk` on the next
alternative, which ultimately returns `Some(a, t)` where `a` is the
next alternative, or `None` if there are no more alternatives. We
repackage this `Option` as a choice of alternatives with `unsplit`. So
that we don't call `fk` too soon, we call `unsplit` via `bind`, which
defers the call until the resulting choice of alternatives is actually
used.

Now we can write infinite choices:

{% highlight scala %}
scala> import LogicSFK._
import LogicSFK._

scala> val nat: T[Int] = unit(0) | nat.map(_ + 1)
nat: LogicSFK.T[Int] = LogicSFK$$anon$3@27aea0c1

scala> run(nat, 10)
res1: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
{% endhighlight %}

Well, this is a pretty complicated way to generate the natural numbers
up to 10...

While `nat` looks like a lazy stream (as you might write in Haskell),
no results are memoized (as they are in Haskell). To compute each
successive number all the previous ones must be recomputed, and the
running time of `run(nat, N)` is O(N<sup>2</sup>).

<b>Defunctionalization</b>

The code above is a fairly direct translation of the Haskell code from
the paper. But its use of continuation-passing style doesn't map well
to Scala, because Scala doesn't implement tail-call elimination
(because the JVM doesn't). Every call to a success or failure
continuation adds a frame to the stack, even though all we ever do
with the result is return it (i.e. the call is in _tail-position_), so
the stack frame could be eliminated.

Surprisingly, we run out of memory before we run out of stack:

{% highlight scala %}
scala> run(nat, 2000)
java.lang.OutOfMemoryError: Java heap space
	...
{% endhighlight %}

A little heap profiling shows that we're using quadratic space as well
as quadratic time. It turns out that the implementation of `Logic.run`
(from the previous post) has a space leak. The call to `run` is not
tail-recursive, so the stack frame hangs around, and although `t` is
dead after `split(t)`, there's still a reference to it on the stack.

We can rewrite `run` with an accumulator to be tail-recursive:

{% highlight scala %}
  def run[A](t: T[A], n: Int): List[A] = {
    def runAcc(t: T[A], n: Int, acc: List[A]): List[A] =
      if (n <= 0) acc.reverse else
        split(t) match {
          case None => acc.reverse
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

To address the stack overflow we turn to _defunctionalization_. The
idea (from John Reynold's classic paper
[Definitional Interpreters for Higher-Order Programming Languages](http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.110.5892&rep=rep1&type=pdf))
is to replace functions and their applications with data constructors
(we'll use case classes) and an `apply` function, which matches the
data constructor and does whatever the corresponding function body
does. If a function captures variables, the data constructor must
capture the same variables.

After defunctionalization we're left with three mutually recursive
`apply` functions (one for each of `T`, `FK`, and `SK`) where each
recursive call is in tail position. In theory the compiler could
transform these into code that takes only constant stack space (since
they are local functions private to `split`). But in fact it will do
so only for single recursive functions, so we will need to do this
transformation by hand.

There is one hitch: the original code is not completely
tail-recursive, because of `unsplit`, which calls a failure
continuation then matches on the result. To fix this we need to add
yet another continuation, which represents what to do after returning
a result from a success or failure continuation.

{% highlight scala %}
object LogicSFKDefunc extends Logic {
  type O[A] = Option[(A,T[A])]

  sealed trait T[A]
  case class Fail[A]() extends T[A]
  case class Unit[A](a: A) extends T[A]
  case class Or[A](t1: T[A], t2: () => T[A]) extends T[A]
  case class Bind[A,B](t: T[A], f: A => T[B]) extends T[B]
  case class Apply[A,B](t: T[A], f: A => B) extends T[B]
  case class Filter[A](t: T[A], p: A => Boolean) extends T[A]
  case class Unsplit[A](fk: FK[O[A]]) extends T[A]

  def fail[A] = Fail()
  def unit[A](a: A) = Unit(a)
  def or[A](t1: T[A], t2: => T[A]) = Or(t1, { () => t2 })
  def bind[A,B](t: T[A], f: A => T[B]) = Bind(t, f)
  def apply[A,B](t: T[A], f: A => B) = Apply(t, f)
  def filter[A](t: T[A], p: A => Boolean) = Filter(t, p)
{% endhighlight %}

A choice of alternatives `T[A]` is now represented symbolically by
case classes, and the functions which operate on choices just return
the corresponding case. The cases capture the same variables that were
captured in the original functions.

We have an additional case `Unsplit` which represents the
`bind(unit(fk), unsplit)` combination from `split`. And we use `O[A]`
as a convenient abbreviation.

{% highlight scala %}
  sealed trait FK[R]
  case class FKOr[A,R](t: () => T[A], sk: SK[A,R], fk: FK[R])
    extends FK[R]
  case class FKSplit[R](r: R) extends FK[R]

  sealed trait SK[A,R]
  case class SKBind[A,B,R](f: A => T[B], sk: SK[B,R])
    extends SK[A,R]
  case class SKApply[A,B,R](f: A => B, sk: SK[B,R])
    extends SK[A,R]
  case class SKFilter[A,R](p: A => Boolean, sk: SK[A,R])
    extends SK[A,R]
  case class SKSplit[A,R](r: (A, FK[R]) => R) extends SK[A,R]

  sealed trait K[R,R2]
  case class KReturn[R]() extends K[R,R]
  case class KUnsplit[A,R,R2](sk: SK[A,R], fk:FK[R], k: K[R,R2])
    extends K[O[A],R2]
{% endhighlight %}

Each case for `FK` (respectively `SK`) corresponds to a success
(respectively failure) continuation function in the original
code---it's easy to match them up.

The `K` cases are for the new return continuation. They are
defunctionalized from functions `R => R2`; we can either return a
value directly, or match on whether it is `Some` or `None` as in
`unsplit`. (If `K` is hard to understand you might try
"refunctionalizing" it by replacing the cases with functions.)

We see that case classes are more powerful than variants in OCaml,
without [GADTs](https://sites.google.com/site/ocamlgadt/) at
least. Cases can have "input" type variables (appearing in arguments)
which do not appear in the "output" (the type the case extends). When
we match on the case these are treated as existentials. And the output
type of a case can be more restrictive than type it extends; when we
match on the case we can make more restrictive assumptions about types
in that branch of the match. More on this in Emir, Odersky, and
Williams'
[Matching Objects with Patterns](http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.88.5295&rep=rep1&type=pdf).

{% highlight scala %}
  def split[A](t: T[A]) = {

    def applyT[A,R,R2]
      (t: T[A], sk: SK[A,R], fk: FK[R], k: K[R,R2]): R2 =
      t match {
        case Fail() => applyFK(fk, k)
        case Unit(a) => applySK(sk, a, fk, k)
        case Or(t1, t2) => applyT(t1, sk, FKOr(t2, sk, fk), k)
        case Bind(t, f) => applyT(t, SKBind(f, sk), fk, k)
        case Apply(t, f) => applyT(t, SKApply(f, sk), fk, k)
        case Filter(t, p) => applyT(t, SKFilter(p, sk), fk, k)
        case Unsplit(fk2) => applyFK(fk2, KUnsplit(sk, fk, k))
      }

    def applyFK[R,R2](fk: FK[R], k: K[R,R2]): R2 =
      fk match {
        case FKOr(t, sk, fk) => applyT(t(), sk, fk, k)
        case FKSplit(r) => applyK(k, r)
      }

    def applySK[A,R,R2]
      (sk: SK[A,R], a: A, fk: FK[R], k: K[R,R2]): R2 =
      sk match {
        case SKBind(f, sk) => applyT(f(a), sk, fk, k)
        case SKApply(f, sk) => applySK(sk, f(a), fk, k)
        case SKFilter(p, sk) =>
          if (p(a)) applySK(sk, a, fk, k) else applyFK(fk, k)
        case SKSplit(rf) => applyK(k, rf(a, fk))
      }
{% endhighlight %}

Again, each of these cases corresponds directly to a function in the
original code, and again it is easy to match them up (modulo the extra
return continuation argument) to see that all we have done is
separated the data part of the function (i.e. the captured variables)
from the code part.

The exception is `Unsplit`, which again corresponds to `bind(unit(fk),
unsplit)`. To apply it, we apply `fk` (which collapses `unit(fk)`,
`bind`, and the application of `fk` in `unsplit`) with `KUnsplit` as
continuation, capturing `sk`, `fk`, and `k` (corresponding to their
capture in the success continuation of `bind`).

{% highlight scala %}
    def applyK[R,R2](k: K[R,R2], r: R): R2 =
      k match {
        case KReturn() => r.asInstanceOf[R2]
        case KUnsplit(sk, fk, k) => {
          r match {
            case None => applyFK(fk, k)
            case Some((a, t)) => applyT(or(unit(a), t), sk, fk, k)
          }
        }
      }
{% endhighlight %}

For `KReturn` we just return the result. Although `KReturn` extends
`K[R,R]`, Scala doesn't deduce from this that `R` = `R2`, so we must
coerce the result. For `KUnsplit` we do the same match as `unsplit`,
then apply the resulting `T` (for the `None` case we call the failure
continuation directly instead of applying `fail`). Here Scala deduces
from the return type of `KUnsplit` that is safe to treat `r` as an
`Option`.

{% highlight scala %}
    applyT[A,O[A],O[A]](
      t,
      SKSplit((a, fk) => Some((a, Unsplit(fk)))),
      FKSplit(None),
      KReturn())
  }
}
{% endhighlight %}

Finally we apply the input `T` in correspondence to the original `split`.

<b>Tail call elimination</b>

(This section has been revised; you can see the original
[here](https://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/ba9621fc48ff84e01d9f70d076cc912b8185729d).)

To eliminate the stack frames from tail calls, we next rewrite the
four mutually-recursive functions into a single recursive function
(which Scala compiles as a loop). To do this we have to abandon some
type safety (but only in the implementation of the `Logic` monad;
we'll still present the same safe interface).

{% highlight scala %}
object LogicSFKDefuncTailrec extends Logic {
  type O[A] = Option[(A,T[A])]

  type T[A] = I

  sealed trait I
  case class Fail() extends I
  case class Unit(a: Any) extends I
  case class Or(t1: I, t2: () => I) extends I
  case class Bind(t: I, f: Any => I) extends I
  case class Apply(t: I, f: Any => Any) extends I
  case class Filter(t: I, p: Any => Boolean) extends I
  case class Unsplit(fk: I) extends I

  case class FKOr(t: () => I, sk: I, fk: I) extends I
  case class FKSplit(r: O[Any]) extends I

  case class SKBind(f: Any => I, sk: I) extends I
  case class SKApply(f: Any => Any, sk: I) extends I
  case class SKFilter(p: Any => Boolean, sk: I) extends I
  case class SKSplit(r: (Any, I) => O[Any]) extends I

  case object KReturn extends I
  case class KUnsplit(sk: I, fk: I, k: I) extends I
{% endhighlight %}

This is all pretty much as before except that we erase all the type
parameters. Having done so we can combine the four defunctionalized
types into a single type `I` (for "instruction" perhaps), which will
allow us to write a single recursive `apply` function. The type
parameter in `T[A]` is then a _phantom type_ since it does not appear
on the right-hand side of the definition; it is used only to enforce
constraints outside the module.

{% highlight scala %}
  def fail[A]: T[A] = Fail()
  def unit[A](a: A): T[A] = Unit(a)
  def or[A](t1: T[A], t2: => T[A]): T[A] = Or(t1, { () => t2 })
  def bind[A,B](t: T[A], f: A => T[B]): T[B] =
    Bind(t, f.asInstanceOf[Any => I])
  def apply[A,B](t: T[A], f: A => B): T[B] =
    Apply(t, f.asInstanceOf[Any => I])
  def filter[A](t: T[A], p: A => Boolean): T[A] =
    Filter(t, p.asInstanceOf[Any => Boolean])
{% endhighlight %}

The functions for building `T[A]` values are mostly the same. We have
to cast passed-in functions since `Any` is not a subtype of arbitrary
`A`. The return type annotations don't seem necessary but I saw some
strange type errors without them (possibly related to the phantom
type?) when using the `Logic.Syntax` wrapper.

{% highlight scala %}
def split[A](t: T[A]): O[A] = {
  def apply(i: I, a: Any, r: O[Any], sk: I, fk: I, k: I): O[Any] =
    i match {
      case Fail() => apply(fk, null, null, null, null, k)
      case Unit(a) => apply(sk, a, null, null, fk, k)
      case Or(t1, t2) =>
        apply(t1, null, null, sk, FKOr(t2, sk, fk), k)
      case Bind(t, f) =>
        apply(t, null, null, SKBind(f, sk), fk, k)
      case Apply(t, f) =>
        apply(t, null, null, SKApply(f, sk), fk, k)
      case Filter(t, p) =>
        apply(t, null, null, SKFilter(p, sk), fk, k)
      case Unsplit(fk2) =>
        apply(fk2, null, null, null, null, KUnsplit(sk, fk, k))

      case FKOr(t, sk, fk) => apply(t(), null, null, sk, fk, k)
      case FKSplit(r) => apply(k, null, r, null, null, null)

      case SKBind(f, sk) => apply(f(a), null, null, sk, fk, k)
      case SKApply(f, sk) => apply(sk, f(a), null, null, fk, k)
      case SKFilter(p, sk) =>
        if (p(a))
          apply(sk, a, null, null, fk, k)
        else
          apply(fk, null, null, null, null, k)
      case SKSplit(rf) =>
        apply(k, null, rf(a, fk), null, null, null)

      case KReturn => r
      case KUnsplit(sk, fk, k) => {
        r match {
          case None => apply(fk, null, null, null, null, k)
          case Some((a, t)) =>
            apply(or(unit(a), t), null, null, sk, fk, k)
        }
      }
    }

  apply(t,
        null,
        null,
        SKSplit((a, fk) => Some((a, Unsplit(fk)))),
        FKSplit(None),
        KReturn).asInstanceOf[O[A]]
}
{% endhighlight %}

The original functions took varying arguments; the single function
takes all the arguments which the original ones did. We pass `null`
for unused arguments in each call, but otherwise the cases are the
same as before.

Now we can evaluate `nat` to large N without running out of stack (but
since the running time is quadratic it takes longer than I care to
wait to complete):

{% highlight scala %}
scala> run(nat, 100000)
^C
{% endhighlight %}

See the complete code
[here](https://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/scala-logic).

Next time we'll thread state through this backtracking logic monad,
and use it to implement unification.
