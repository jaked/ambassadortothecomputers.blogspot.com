---
layout: post
title: Logic programming in Scala part 3, unification
---

In this post I want to build on the backtracking logic monad we
covered
[last time](/2011/04/logic-programming-in-scala-part-2.html)
by adding _unification_, yielding an embedded DSL for Prolog-style
logic programming.

<b>Prolog</b>

Here is a small Prolog example, the rough equivalent of
`List.contains` in Scala:

{% highlight prolog %}
  member(X, [X|T]).
  member(X, [_|T]) :- member(X, T).
{% endhighlight %}

`Member` doesn't return a boolean; instead it succeeds or fails (in
the same way as the logic monad). The _goal_ `member(1, [1,2,3])`
succeeds; the goal `member(4, [1,2,3])` fails. (What happens for
`member(1, [1,1,3])`?)

A Prolog _predicate_ is defined by one or more _clauses_ (each ending
in a period), made up of a _head_ (the predicate and arguments before
the `:-`) and zero or more _subgoals_ (goals after the `:-`, separated
by commas; if there are no subgoals the `:-` is omitted). To solve a
goal, we _unify_ it (match it) with each clause head, then solve each
subgoal in the clause. If a subgoal fails we backtrack and try the
next matching head; if there is no matching head the goal fails. A
goal may succeed more than once.

For `member` we have two clauses: the first says that `member`
succeeds if `X` is the head of the list (`[X|T]` is the same as `x::t`
in Scala); the second says that `member` succeeds if `X` is a member
of the tail of the list, regardless of the head. There is no clause
where the list is empty (written `[]`); a goal with an empty list
fails since there is no matching clause head.

Prolog unification is more expressive than pattern matching as found
in Scala, OCaml, etc. Both sides of a unification may contain
variables; unification attempts to instantiate them so that the two
sides are equal. Variables are instantiated by _terms_, which
themselves may contain variables; unification finds the most general
instantiation which makes the sides equal.

As a small example of this expressivity, we can run `member`
"backwards": the goal `member(X, [1,2,3])` succeeds once for each
element of the list, with `X` bound to the element.

There is much more on Prolog and logic programming in
[Frank Pfenning's course notes](http://www.cs.cmu.edu/~fp/courses/lp/lectures/lp-all.pdf), which I recommend highly.

<b>Unification</b>

For each type we want to use in unification we'll define a
corresponding type of terms, which have the same structure as the
underlying type but can also contain variables. These aren't Scala
variables (which of course can't be stored in a data structure) but
"existential variables", or _evars_. Evars are just tags; computations
will carry an _environment_ mapping evars to terms, which may be
updated after a successful unification.

{% highlight scala %}
import scala.collection.immutable.{Map,HashMap}

class Evar[A](val name: String)
object Evar { def apply[A](name: String) = new Evar[A](name) }

trait Term[A] {
  // invariant: on call to unify, this and t have e substituted
  def unify(e: Env, t: Term[A]): Option[Env]

  def occurs[B](v: Evar[B]): Boolean
  def subst(e: Env): Term[A]
  def ground: A
}
{% endhighlight %}

The important property of an evar is that it is distinct from every
other evar; the name attached to it is just a label. An evar is
indexed by a phantom type indicating the underlying type of terms
which may be bound to it.

A term is indexed by its underlying type. So `Int` becomes
`Term[Int]`, `String` becomes `Term[String]`, and so on; an evar of
type `Evar[A]` may only be bound to a term of type `Term[A]`. (Prolog
is dynamically typed, but this statically-typed treatment of evars and
terms fits better with Scala.)

The `unify` method unifies a term with another term of the same type,
taking an environment and returning an updated environment (or `None`
if the unification fails). `Occurs` checks if an evar occurs in a term
(as we will see this is used to prevent circular bindings). `Subst`
substitutes the variables in a term with their bindings in an
environment, and `ground` returns the underlying Scala value
represented by the term (provided the term contains no evars).

{% highlight scala %}
class Env(m: Map[Evar[Any],Term[Any]]) {
  def apply[A](v: Evar[A]) =
    m(v.asInstanceOf[Evar[Any]]).asInstanceOf[Term[A]]
  def get[A](v: Evar[A]): Option[Term[A]] =
    m.get(v.asInstanceOf[Evar[Any]]).asInstanceOf[Option[Term[A]]]
  def updated[A](v: Evar[A], t: Term[A]): Env = {
    val v2 = v.asInstanceOf[Evar[Any]]
    val t2 = t.asInstanceOf[Term[Any]]
    val e2 = Env(Map(v2 -> t2))
    val m2 = m.mapValues(_.subst(e2))
    Env(m2.updated(v2, t2))
  }
}
object Env {
  def apply(m: Map[Evar[Any],Term[Any]]) = new Env(m)
  def empty = new Env(HashMap())
}
{% endhighlight %}

An environment is just a map from evars to terms. Because we need to
store evars and terms of different types in the same environment, we
cast them to and from `Any`; this is safe because of the phantom type
on `Evar`. For simplicity we maintain the invariant that the term
bound to each evar is already substituted by the rest of the
environment.

{% highlight scala %}
case class VarTerm[A](v: Evar[A]) extends Term[A] {
  def unify(e: Env, t: Term[A]) =
    t match {
      case VarTerm(v2) if (v2 == v) => Some(e)
      case _ =>
        if (t.occurs(v)) None
        else Some(e.updated(v, t))
    }

  def occurs[B](v2: Evar[B]) = v2 == v

  def subst(e: Env) =
    e.get(v) match {
      case Some(t) => t
      case None => this
    }

  def ground =
    throw new IllegalArgumentException("not ground")

  override def toString = { v.name  }
}
{% endhighlight %}

The `VarTerm` class represents terms consisting of an evar. To unify a
`VarTerm` with another `VarTerm` containing the same evar, we just
return the environment unchanged (since there is no new
information). Otherwise we check that the evar doesn't appear in the
term (since a unification `x =:= List(x)` would create a circular
term) then return the updated environment.

To substitute a `VarTerm` we return the term bound to the evar in the
environment if one exists, otherwise the unsubstituted `VarTerm`. A
`VarTerm` is never ground (we assume `ground` is called only on terms
which are already substituted by the environment).

{% highlight scala %}
case class LitTerm[A](a: A) extends Term[A] {
  def unify(e: Env, t: Term[A]) =
    t match {
      case LitTerm(a2) => if (a == a2) Some(e) else None
      case _: VarTerm[_] => t.unify(e, this)
      case _ => None
    }

  def occurs[B](v: Evar[B]) = false
  def subst(e: Env) = this
  def ground = a

  override def toString = { a.toString }
}
{% endhighlight %}

`LitTerm` represents terms of literal Scala values. A `LitTerm`
unifies with another `LitTerm` containing an equal value, but that
adds nothing to the environment. Then we have two cases which we need
for every term type---to unify with a `VarTerm` call `unify` back on
it; otherwise fail.

{% highlight scala %}
case class NilTerm[A]() extends Term[List[A]] {
  def unify(e: Env, t: Term[List[A]]) =
    t match {
      case NilTerm() => Some(e)
      case _: VarTerm[_] => t.unify(e, this)
      case _ => None
    }

  def occurs[B](v: Evar[B]) = false
  def subst(e: Env) = this
  def ground = Nil

  override def toString = { Nil.toString }
}

case class ConsTerm[A](hd: Term[A], tl: Term[List[A]])
  extends Term[List[A]]
{
  def unify(e: Env, t: Term[List[A]]) =
    t match {
      case ConsTerm(hd2, tl2) =>
        for {
          e1 <- hd.unify(e, hd2)
          e2 <- tl.subst(e1).unify(e1, tl2.subst(e1))
        } yield e2
      case _: VarTerm[_] => t.unify(e, this)
      case _ => None
    }

  def occurs[C](v: Evar[C]) = hd.occurs(v) || tl.occurs(v)
  def subst(e: Env) = ConsTerm(hd.subst(e), tl.subst(e))
  def ground = hd.ground :: tl.ground

  override def toString = { hd.toString + " :: " + tl.toString }
}
{% endhighlight %}

`NilTerm` and `ConsTerm` represent the `Nil` and `::` constructors for
lists. `Nil` is sort of like a literal, so the methods for `NilTerm`
are similar to those for `LitTerm`. For `ConsTerm` we unify by
unifying the heads and tails, calling `subst` on the tails since
unifying the heads may have added bindings to the environment. (Here
it's convenient to use a for-comprehension on the `Option[Env]` type
since either unification may fail.) Similarly we implement `occurs`,
`subst`, and `ground` by calling them on the head and tail.

{% highlight scala %}
object Term {
  implicit def var2Term[A](v: Evar[A]): Term[A] = VarTerm(v)
  //implicit def lit2term[A](a: A): Term[A] = LitTerm(a)
  implicit def int2Term(a: Int): Term[Int] = LitTerm(a)
  implicit def list2Term[A](l: List[Term[A]]): Term[List[A]] =
    l match {
      case Nil => NilTerm[A]
      case hd :: tl => ConsTerm(hd, list2Term(tl))
    }
}
{% endhighlight %}

Finally we have some implicit conversions to make it a little easier
to build `Term` values. The `lit2term` conversion turned out to be a
bad idea; in particular you don't want a `LitTerm[List[A]]` since it
doesn't unify with a `ConsTerm[A]` or `NilTerm[A]`.

<b>State</b>

In order to combine unification with backtracking, we need to keep
track of the environment along each branch of the tree of choices. We
don't want the environments from different branches to interfere, so
it's convenient to use a purely functional environment representation;
we pass the current environment down the tree as computation
proceeds. However, we can hide this state passing in the monad
interface:

{% highlight scala %}
trait LogicState { L =>
  type T[S,A]
  // as before
  def split[S,A](s: S, t: T[S,A]): Option[(S,A,T[S,A])]

  def get[S]: T[S,S]
  def set[S](s: S): T[S, Unit]

  case class Syntax[S,A](t: T[S,A]) {
    // as before
    def &[B](t2: => T[S,B]): T[S,B] = L.bind(t, { _: A => t2 })
  }
}
{% endhighlight %}

`LogicState` is mostly the same as `Logic`, except that the type of
choices has an extra parameter for the type of the state. The `get`
and `set` functions get and set the current state. To `split` we need
an initial state to get things started, and each result includes an
updated state. Finally we add the syntax `&` to sequence
two computations, ignoring the value of the first. We'll use this to
sequence goals, since we care only about the updated environment.

The simplest implementation of `LogicState` builds on `Logic`:

{% highlight scala %}
trait LogicStateT extends LogicState {
  val Logic: Logic

  type T[S,A] = S => Logic.T[(S, A)]
{% endhighlight %}

We embed state-passing in a `Logic.T` as a function from an initial
state to a choice of alternatives, where each alternative includes an
updated state along with its value.

{% highlight scala %}
  def fail[S,A] = { s: S => Logic.fail }
  def unit[S,A](a: A) = { s: S => Logic.unit((s, a)) }

  def or[S,A](t1: T[S,A], t2: => T[S,A]) =
    { s: S => Logic.or(t1(s), t2(s)) }

  def bind[S,A,B](t: T[S,A], f: A => T[S,B]) = {
    val f2: ((S,A)) => Logic.T[(S,B)] = { case (s, a) => f(a)(s) }
    { s: S => Logic.bind(t(s), f2) }
  }

  def apply[S,A,B](t: T[S,A], f: A => B) = {
    val f2: ((S,A)) => ((S,B)) = { case (s, a) => (s, f(a)) }
    { s: S => Logic.apply(t(s), f2) }
  }

  def filter[S,A](t: T[S,A], p: A => Boolean) = {
    val p2: ((S,A)) => Boolean = { case (_, a) => p(a) }
    { s: S => Logic.filter(t(s), p2) }
  }
{% endhighlight %}

All of these operations pass the state through unchanged. Note that
`or` passes the same state to both alternatives---different branches
of the tree cannot interfere with one another's state.

{% highlight scala %}
  def split[S,A](s: S, t: T[S,A]) = {
    Logic.split(t(s)) match {
      case None => None
      case Some(((s, a), t)) => Some((s, a, { _ => t }))
    }
  }

  def get[S] = { s: S => Logic.unit((s,s)) }
  def set[S](s: S) = { _: S => Logic.unit((s,())) }
}
{% endhighlight %}

In `split` we pass the given state to the underlying `Logic.T`, and
for each alternative we unpack the pair of state and value. The choice
of remaining alternatives `t` encapsulates the current state, so when
we return it we ignore the input state. In `get` and `set` we return
and replace the current state.

Another approach is to pass state explicitly through `LogicSFK`:

{% highlight scala %}
object LogicStateSFK extends LogicState {
  type FK[R] = () => R
  type SK[S,A,R] = (S, A, FK[R]) => R

  trait T[S,A] { def apply[R](s: S, sk: SK[S,A,R], fk: FK[R]): R }
{% endhighlight %}

This is not really any different from `LogicStateT` applied to
`LogicSFK`---we have just uncurried the state argument. We can take
the same path as last time and defunctionalize this into a
tail-recursive implementation
(see the [full code](https://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/scala-logic))
although `LogicStateT` applied to `LogicSFKDefuncTailrec` inherits
tail-recursiveness from the underlying `Logic` monad.

<b>Scrolog</b>

Finally we can put the pieces together into a Prolog-like embedded
DSL:

{% highlight scala %}
trait Scrolog {
  val LogicState: LogicState
  import LogicState._

  type G = T[Env,Unit]
{% endhighlight %}

From our point of view, a goal is a stateful choice among
alternatives, where we don't care about the value returned, only the
environment.

{% highlight scala %}
  class TermSyntax[A](t: Term[A]) {
    def =:=(t2: Term[A]): G =
      for {
        env <- get
        env2 <- {
          t.subst(env).unify(env, t2.subst(env)) match {
            case None => fail[Env,Unit]
            case Some(e) => set(e)
          }
        }
      } yield env2
  }

  implicit def termSyntax[A](t: Term[A]) = new TermSyntax(t)
  implicit def syntax[A](t: G) = LogicState.syntax(t)
{% endhighlight %}

We connect term unification to the stateful logic monad with a wrapper
class defining a `=:=` operator. To unify terms in the monad, we get
the current environment, substitute it into the two terms (to satisfy
the invariant above), then call `unify`; if it fails we fail the
computation, else we set the new state.

{% highlight scala %}
  def run[A](t: G, n: Int, tm: Term[A]): List[Term[A]] =
    LogicState.run(Env.empty, t, n)
      .map({ case (e, _) => tm.subst(e) })
}
{% endhighlight %}

The `run` function solves a goal, taking as arguments the goal, the
maximum number of solutions to find, and a term to be evaluated in the
environment of each solution.

<b>Examples</b>

First we need to set up Scrolog:

{% highlight scala %}
val Scrolog =
  new Scrolog { val LogicState =
    new LogicStateT { val Logic = LogicSFKDefuncTailrec }
  }
import Scrolog._
{% endhighlight %}

Here is a translation of the `member` predicate:

{% highlight scala %}
  def member[A](x: Term[A], l: Term[List[A]]): G = {
    val hd = Evar[A]("hd"); val tl = Evar[List[A]]("tl")
    ConsTerm(x, tl) =:= l |
    (ConsTerm(hd, tl) =:= l & member(x, tl))
  }
{% endhighlight %}

We implement predicates by functions, and goals by function calls. To
implement matching the clause head, we explicitly unify the input
arguments against each clause head, and combine the clauses with
`|`. Subgoals are sequenced with `&`. Finally, we must create local
evars explicitly, since they are fresh for each call (just as local
variables are in Scala).

Finally we can run the goal above:

{% highlight scala %}
scala> val x = Evar[Int]("x")
scala> run(member(x, List[Term[Int]](1, 2, 3)), 3, x)
res6: List[Term[Int]] = List(1, 2, 3)
{% endhighlight %}

As another example, we can implement addition over unary natural
numbers. In Prolog this would be

{% highlight prolog %}
  sum(Z, N, N).
  sum(s(M), N, s(P)) :- sum(M, N, P).
{% endhighlight %}

In Prolog we can just invent symbols like `s` and `z`; in Scala we
need first to define a type of natural numbers, then terms over that
type:

{% highlight scala %}
  sealed trait Nat
  case object Z extends Nat
  case class S(n: Nat) extends Nat

  case object ZTerm extends Term[Nat] {
    // like NilTerm

  case class STerm(n: Term[Nat]) extends Term[Nat] {
    // like ConsTerm
{% endhighlight %}

Then we can define `sum`, again separating the clauses by `|` and
explicitly unifying the clause heads:

{% highlight scala %}
  def sum(m: Term[Nat], n: Term[Nat], p: Term[Nat]): G = {
    val m2 = Evar[Nat]("m"); val p2 = Evar[Nat]("p")
    (m =:= Z & n =:= p) |
    (m =:= STerm(m2) & p =:= STerm(p2) & sum(m2, n, p2))
  }
{% endhighlight %}

We can use `sum` to do addition:

{% highlight scala %}
scala> val x = Evar[Nat]("x"); val y = Evar[Nat]("y")
scala> run(sum(S(Z), S(S(Z)), x), 1, x)
res8: List[Term[Nat]] = List(S(S(S(Z))))
{% endhighlight %}

or subtraction:

{% highlight scala %}
scala> run(sum(x, S(S(Z)), S(S(S(Z)))), 1, x)
res10: List[Term[Nat]] = List(S(Z))

scala> run(sum(S(Z), x, S(S(S(Z)))), 1, x)
res11: List[Term[Nat]] = List(S(S(Z)))
{% endhighlight %}

or even to find all the pairs of naturals which sum to 3:

{% highlight scala %}
scala> run(sum(x, y, S(S(S(Z)))), 10, List[Term[Nat]](x, y))
res14: List[Term[List[Nat]]] =
  List(Z :: S(S(S(Z))) :: List(),
       S(Z) :: S(S(Z)) :: List(),
       S(S(Z)) :: S(Z) :: List(),
       S(S(S(Z))) :: Z :: List())
{% endhighlight %}

although the printing of `Term[List]` could be better.

This is only a small taste of the expressivity of Prolog-style logic
programming. Again let me recommend
[Frank Pfenning's course notes](http://www.cs.cmu.edu/~fp/courses/lp/lectures/lp-all.pdf),
which explore the semantics of Prolog in a "definitional interpreters"
style, by gradually refining an interpreter to expose more of the
machinery of the language.

See the [full code](https://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/scala-logic).
