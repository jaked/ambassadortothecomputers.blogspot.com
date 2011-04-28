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
      def apply[R](sk: SK[A,R], fk: FK[R]) =
        t1(sk, { () => t2(sk, fk) })
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
      def apply[R](sk: SK[A,R], fk: FK[R]) = {
        val sk2: SK[A,R] =
          { (a, fk) => if (p(a)) sk(a, fk) else fk() }
        t(sk2, fk)
      }
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

<b>Defunctionalization</b>

The code above is a pretty direct translation of the Haskell code from
the paper. But its use of continuation-passing style doesn't map well
to Scala because Scala (because of the JVM) doesn't implement
tail-call elimination. Every call to a success or failure continuation
adds a frame to the stack, even though all we ever do with the result
of those calls is return it (that is, the calls are always in tail
position) and the stack frame could be eliminated.

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
          case None => acc
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

To address the stack problem we turn to *defunctionalization*. The
idea (from John Reynold's classic paper
[Definitional Interpreters for Higher-Order Programming Languages](http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.110.5892&rep=rep1&type=pdf))
is to replace functions and their applications with data constructors
and an `apply` function, which matches the data constructor and does
whatever the corresponding function body does. (If a function captures
variables, the data constructor must capture the same variables.)

We systematically defunctionalize our types `T`, `FK`, and `SK`
(represented as functions in `LogicSFK`), replacing them by case
classes and mutually-recursive `apply` functions where all calls are
tail-calls, which can in theory be locally transformed into a loop, so
`split` can run in constant stack space. Unfortunately the Scala
compiler does not do this transformation, so we will need to do it
manually.

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
the corresponding case. As promised, the cases capture the same
variables that were captured in the original functions.

We have an additional case `Unsplit` which represents the
`bind(unit(fk), unsplit)` combination in `split`. Finally we have
`O[A]` as a convenient abbreviation.

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

`FK` and `SK` are defunctionalized as well; each case corresponds to
(and captures the same variables as) a function in the original
code. The `K` cases are for the "yet another" continuation, and
represent functions `R => R2`; we can either return a value directly,
or match on whether it is `Some` or `None`, as in `unsplit`.

We see here that case classes are more powerful than variants in OCaml
(without [GADTs](https://sites.google.com/site/ocamlgadt/) at
least). For several cases we have "input" type variables (appearing in
arguments) which do not appear in the "output" (the type the case
extends). When we decompose the case these are in effect existentials:
there is some type but we don't know what it is. For the `K` classes
the output types are more restrictive than the `K` trait; when we
decompose them we can make use of those restrictions.

You might find `split` easier to understand reading from the bottom
up:

{% highlight scala %}
  def split[A](t: T[A]) = {

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

To apply a "yet another" continuation: For `KReturn` we just return
the result; we must coerce it because although `KReturn` extends
`K[R,R]`, Scala doesn't use of this information to conclude that `R` =
`R2` (this seems like a bug). For `KUnsplit` we do the same match as
`unsplit`, then apply the resulting `T` (for the `None` case we call
the failure continuation directly instead of applying `fail`). Here
Scala uses the return type of `KUnsplit` to see that is safe to treat
`r` as an `Option`.

{% highlight scala %}
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

All of these cases correspond directly to functions in
`LogicSFK`---you should be able to see the correspondence by comparing
a few of them.

The exception is the `Unsplit`, which again corresponds to
`bind(unit(fk), unsplit)`. To apply it, we apply `fk` (which collapses
`unit(fk)`, `bind`, and the application of `fk` in `unsplit`) with
`KUnsplit` as continuation, capturing `sk`, `fk`, and `k`
(corresponding to their capture in the success continuation of
`bind`).

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

<b>Tail recursion</b>

Scala doesn't optimize the tail calls in the mutually-recursive
`apply` functions above---it handles only single recursive
functions---but we can hand-optimize them into a single loop. To do
this we have to abandon some type safety, but only in the
implementation of the `Logic` monad; we'll still present the same safe
interface.

{% highlight scala %}
object LogicSFKDefuncTailrec extends Logic {
  type O[A] = Option[(A,T[A])]

  sealed trait T[A]
  case class Fail[A]() extends T[A]
  case class Unit[A](a: A) extends T[A]
  case class Or[A](t1: T[A], t2: () => T[A]) extends T[A]
  case class Bind[A,B](t: T[A], f: A => T[B]) extends T[B]
  case class Apply[A,B](t: T[A], f: A => B) extends T[B]
  case class Filter[A](t: T[A], p: A => Boolean) extends T[A]
  case class Unsplit[A](fk: FK) extends T[A]

  def fail[A] = Fail()
  def unit[A](a: A) = Unit(a)
  def or[A](t1: T[A], t2: => T[A]) = Or(t1, { () => t2 })
  def bind[A,B](t: T[A], f: A => T[B]) = Bind(t, f)
  def apply[A,B](t: T[A], f: A => B) = Apply(t, f)
  def filter[A](t: T[A], p: A => Boolean) = Filter(t, p)

  sealed trait FK
  case class FKOr(t: () => T[Any], sk: SK, fk: FK) extends FK
  case class FKSplit(r: O[Any]) extends FK

  sealed trait SK
  case class SKBind(f: Any => T[Any], sk: SK) extends SK
  case class SKApply(f: Any => Any, sk: SK) extends SK
  case class SKFilter(p: Any => Boolean, sk: SK) extends SK
  case class SKSplit(r: (Any, FK) => O[Any]) extends SK

  sealed trait K
  case object KReturn extends K
  case class KUnsplit(sk: SK, fk: FK, k: K) extends K
{% endhighlight %}

This is all as before except that we erase the type variables on the
continuations to `Any`. We won't be able to maintain full types in
`split` because we'll use a common set of variables to represent the
function arguments for all four `apply` functions.

{% highlight scala %}
  def split[A](t2: T[A]): O[A] = {
    var app = 1
    var t = t2.asInstanceOf[T[Any]]
    var a: Any = null
    var r: O[Any] = null
    var sk: SK = SKSplit((a, fk) => Some((a, Unsplit(fk))))
    var fk: FK = FKSplit(None)
    var k: K = KReturn

    while (true) {
      (app: @annotation.switch) match {
        case 0 => // applyK
          k match {
            case KReturn => return r.asInstanceOf[O[A]]
            case KUnsplit(sk2, fk2, k2) =>
              r match {
                case None => { app = 2; fk = fk2; k = k2 }
                case Some((a2, t2)) => {
                  app = 1; t = or(unit(a2), t2); sk = sk2
                  fk = fk2; k = k2
                }
              }
          }

        case 1 => // applyT
          t match {
            case Fail() => app = 2
            case Unit(a2) => { a = a2; app = 3 }
            case Or(t1, t2) => { t = t1; fk = FKOr(t2, sk, fk) }
            case Bind(t2, f) => { t = t2; sk = SKBind(f, sk) }
            case Apply(t2, f) => { t = t2; sk = SKApply(f, sk) }
            case Filter(t2, p) => { t = t2; sk = SKFilter(p, sk) }
            case Unsplit(fk2) => {
              app = 2; k = KUnsplit(sk, fk, k); fk = fk2
            }
          }

        case 2 => // applyFK
          fk match {
            case FKOr(t2, sk2, fk2) => {
              app = 1; t = t2(); sk = sk2; fk = fk2
            }
            case FKSplit(r2) => { app = 0; r = r2 }
          }

        case 3 => // applySK
          sk match {
            case SKBind(f, sk2) => { app = 1; t = f(a); sk = sk2 }
            case SKApply(f, sk2) => { sk = sk2; a = f(a) }
            case SKFilter(p, sk2) =>
              if (p(a)) sk = sk2 else app = 2
            case SKSplit(rf) => { app = 0; r = rf(a, fk) }
          }
      }
    }
    throw new Exception("not reached")
  }
}
{% endhighlight %}

Here we translate the mutally-recursive `apply` functions into a
single `while` loop. The arguments for all the functions are kept in a
set of `var`s, with an additional argument `app` to indicate which
function we're in. To tail-call a function we just set the appropriate
argument variables, set `app` to the function we want to call, then go
around the loop again. (In cases where the new value of an argument is
always the same as the old value, we don't even need to set it.) The
result evidently uses only constant stack space.

We can use the `switch` annotation to be sure that the match on `app`
is compiled to a JVM-level jump table. Unfortunately Scala doesn't
compile a match on a set of case classes into a jump table, even
though for sealed classes it would be safe to do so. We could
implement this by hand using an extra tag field on each case.

Next time we'll thread state through this backtracking logic monad,
and use it to implement unification.
