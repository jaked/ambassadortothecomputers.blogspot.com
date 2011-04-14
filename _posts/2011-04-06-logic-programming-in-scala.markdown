---
layout: post
title: Logic programming in Scala
---

I got a new job where I am hacking some Scala. I thought I would learn
something by translating some functional code into Scala, and a friend
had recently pointed me to Kiselyov et al.'s
[Backtracking, Interleaving, and Terminating Monad Transformers](http://okmij.org/ftp/Computation/LogicT.pdf), which
provides a foundation for Prolog-style logic programming. Of course, a
good translation should use the local idiom. So in this post (and the
next) I want to explore an embedded domain-specific language for logic
programming in Scala.

<b>A search problem</b>

Here is a problem I sometimes give in interviews:

> Four people need to cross a rickety bridge, which can hold only two
> people at a time. It's a moonless night, so they need a light to
> cross; they have one flashlight with a battery which lasts 60
> minutes. Each person crosses the bridge at a different speed: Alice
> takes 5 minutes, Bob takes 10, Candace takes 20 minutes, and Dave
> 25. How do they get across?

I'm not interested in the answer---I'm interviewing programmers, not
law school applicants---but rather in how to write a program to find
the answer.

The basic shape of the solution is to represent the state of the world
(where are the people, where is the flashlight, how much battery is
left), write a function to compute from any particular state the set
of possible next states, then search for an answer (a path from the
start state to the final state) in the tree formed by applying the
next state function transitively to the start state.
([Here is a paper](http://web.engr.oregonstate.edu/~erwig/papers/Zurg_JFP04.pdf)
describing solutions in Prolog and Haskell.)

Here is a first solution in Scala:

{% highlight scala %}
object Bridge0 {
  object Person extends Enumeration {
    type Person = Value
    val Alice, Bob, Candace, Dave = Value
    val all = List(Alice, Bob, Candace, Dave) // values is broken
  }
  import Person._

  val times = Map(Alice -> 5, Bob -> 10, Candace -> 20, Dave -> 25)

  case class State(left: List[Person],
                   lightOnLeft: Boolean,
                   timeRemaining: Int)
{% endhighlight %}

We define an enumeration of people (the `Enumeration` class is a [bit
broken](https://lampsvn.epfl.ch/trac/scala/ticket/3687) in Scala
2.8.1), a map of the time each takes to cross, and a case class to
store the state of the world: the list of people on the left side of
the bridge (the right side is just the complement); whether the
flashlight is on the left; and how much time remains in the
flashlight.

{% highlight scala %}
  def chooseTwo(list: List[Person]): List[(Person,Person)] = {
    val init: List[(Person, Person)] = Nil
    list.foldLeft(init) { (pairs, p1) =>
      list.foldLeft(pairs) { (pairs, p2) =>
        if (p1 < p2) (p1, p2) :: pairs else pairs
      }
    }
  }
{% endhighlight %}

This function returns the list of pairs of people from the input
list. We use `foldLeft` to do a double loop over the input list,
accumulating pairs `(p1, p2)` where `p1 < p2`; this avoids returning
`(Alice, Bob)` and also `(Bob, Alice)`. The use of `foldLeft` is
rather OCamlish, and if you know Scala you will complain that
`foldLeft` is not idiomatic---we will repair this shortly.

In Scala, `Nil` doesn't have type `'a list` like in OCaml and Haskell,
but rather `List[Nothing]`. The way local type inference works, the
type variable in the type of `foldLeft` is instantiated with the type
of the `init` argument, so you have to ascribe a type to `init` (or
explicitly instantiate the type variable with `foldLeft[List[(Person,
Person)]]`) or else you get a type clash between `List[Nothing]` and
`List[(Person, Person)]`.

{% highlight scala %}
  def next(state: State): List[State] = {
    if (state.lightOnLeft) {
      val init: List[State] = Nil
      chooseTwo(state.left).foldLeft(init) {
        case (states, (p1, p2)) =>
          val timeRemaining =
            state.timeRemaining - math.max(times(p1), times(p2))
          if (timeRemaining >= 0) {
            val left =
              state.left.filterNot { p => p == p1 || p == p2 }
            State(left, false, timeRemaining) :: states
          }
          else
            states
      }
    } else {
      val right = Person.all.filterNot(state.left.contains)
      val init: List[State] = Nil
      right.foldLeft(init) { (states, p) =>
        val timeRemaining = state.timeRemaining - times(p)
        if (timeRemaining >= 0)
          State(p :: state.left, true, timeRemaining) :: states
        else
          states
      }
    }
  }
{% endhighlight %}

Here we compute the set of successor states for a state. We make a
heuristic simplification: when the flashlight is on the left (the side
where everyone begins) we move two people from the left to the right;
when it is on the right we move only one. I don't have a proof that an
answer must take this form, but I believe it, and it makes the code
shorter.

So when the light is on the left we fold over all the pairs of people
still on the left, compute the time remaining if they were to cross,
and if it is not negative build a new state where they and the
flashlight are moved to the right and the time remaining updated.

If the light is on the right we do the same in reverse, but choose
only one person to move.

{% highlight scala %}
  def tree(path: List[State]): List[List[State]] =
    next(path.head).
      map(s => tree(s :: path)).
        foldLeft(List(path)) { _ ++ _ }

  def search: List[List[State]] = {
    val start = List(State(Person.all, true, 60))
    tree(start).filter { _.head.left == Nil }
  }
}
{% endhighlight %}

A list of successive states is a _path_ (with the starting state at
the end and the most recent state at the beginning); the state tree is
a set of paths. The tree rooted at a path is the set of paths with the
input path as a suffix. To compute this tree, we find the successor
states of the head of the path, augment the path with each state in
turn, recursively find the tree rooted at each augmented path, then
append them all (including the input path).

Then to find an answer, we generate the state tree rooted at the path
consisting only of the start state (everybody and the flashlight on
the left, 60 minutes remaining on the light), then filter out the
paths which end in a final state (everybody on the right).

<b>For-comprehensions</b>

To make the code above more idiomatic Scala (and more readable), we
would of course use for-comprehensions, for example:

{% highlight scala %}
  def chooseTwo(list: List[Person]): List[(Person,Person)] =
    for { p1 <- list; p2 <- list; if p1 < p2 } yield (p1, p2)
{% endhighlight %}

Just as before, we do a double loop over the input list, returning
pairs where `p1 < p2`. (However, under the hood the result list is
constructed by appending to a `ListBuffer` rather than with `::`, so
the pairs are returned in the reverse order.)

The for-comprehension syntax isn't specific to lists. It's syntactic
sugar which translates to method calls, so we can use it on any
objects which implement the right methods. The methods we need are

{% highlight scala %}
  def filter(p: A => Boolean): T[A]
  def map[B](f: A => B): T[B]
  def flatMap[B](f: A => T[B]): T[B]
  def withFilter(p: A => Boolean): T[A]
{% endhighlight %}

where `T` is some type constructor, like `List`. For `List`, `filter` and
`map` have their ordinary meaning, and `flatMap` is a `map` (where
the result type must be a list) which concatenates the resulting lists
(that is, it flattens the list of lists).

`WithFilter` is like `filter` but should be implemented as a "virtual"
filter for efficiency---for `List` it doesn't build a new filtered
list, but instead just keeps track of the filter function; this way
multiple adjacent filters can be combined and the result produced with
a single pass over the list.

The details of the translation are in the [Scala reference
manual](http://www.scala-lang.org/docu/files/ScalaReference.pdf),
section 6.19. Roughly speaking, `<-` becomes `flatMap`, `if` becomes
`filter`, and `yield` becomes `map`. So another way to write
`chooseTwo` is:

{% highlight scala %}
  def chooseTwo(list: List[Person]): List[(Person,Person)] =
    list.flatMap(p1 =>
      list.filter(p2 => p1 < p2).map(p2 => (p1, p2)))
{% endhighlight %}

<b>The logic monad</b>

So far we have taken a concrete view of the choices that arise in
searching the state tree, by representing a choice among alternatives
as a list. For example, in the `chooseTwo` function we returned a list
of alternative pairs. I want now to take a more abstract view, and
define an abstract type `T[A]` to represent a choice among
alternatives of type `A`, along with operations on the type, packaged
into a trait:

{% highlight scala %}
trait Logic { L =>
  type T[A]

  def fail[A]: T[A]
  def unit[A](a: A): T[A]
  def or[A](t1: T[A], t2: => T[A]): T[A]
  def apply[A,B](t: T[A], f: A => B): T[B]
  def bind[A,B](t: T[A], f: A => T[B]): T[B]
  def filter[A](t: T[A], p: A => Boolean): T[A]
  def split[A](t: T[A]): Option[(A,T[A])]
{% endhighlight %}

A `fail` value is a choice among no alternatives. A `unit(a)` is a
choice of a single alternative. The value `or(t1, t2)` is a choice
among the alternatives represented by `t1` together with the
alternatives represented by `t2`.

The meaning of `apply`ing a function to a choice of alternatives is a
choice among the results of applying the function to each alternative;
that is, if `t` represents a choice among `1`, `2`, and `3`, then
`apply(t, f)` represents a choice among `f(1)`, `f(2)`, and `f(3)`.

`Bind` is the same except the function returns a choice of
alternatives, so we must combine all the alternatives in the result;
that is, if `t` is a choice among `1`, `3`, and `5`, and `f` is
`{ x => or(unit(x), unit(x + 1)) }`, then `bind(t, f)` is a choice among
`1`, `2`, `3`, `4`, `5`, and `6`.

A `filter` of a choice of alternatives by a predicate is a choice
among only the alternatives which pass the the predicate.

Finally, `split` is a function which returns the first alternative in
a choice of alternatives (if there is at least one) along with a
choice among the remaining alternatives.

{% highlight scala %}
  def or[A](as: List[A]): T[A] =
    as.foldRight(fail[A])((a, t) => or(unit(a), t))

  def run[A](t: T[A], n: Int): List[A] =
    if (n <= 0) Nil else
      split(t) match {
        case None => Nil
        case Some((a, t)) => a :: run(t, n - 1)
      }
{% endhighlight %}

As a convenience, `or(as: List[A])` means a choice among the elements
of `as`. And `run` returns a list of the first `n` alternatives in a
choice, picking them off one by one with `split`; this is how we get
answers out of a `T[A]`.

{% highlight scala %}
  case class Syntax[A](t: T[A]) {
    def map[B](f: A => B): T[B] = L.apply(t, f)
    def filter(p: A => Boolean): T[A] = L.filter(t, p)
    def flatMap[B](f: A => T[B]): T[B] = L.bind(t, f)
    def withFilter(p: A => Boolean): T[A] = L.filter(t, p)

    def |(t2: => T[A]): T[A] = L.or(t, t2)
  }

  implicit def syntax[A](t: T[A]) = Syntax(t)
}
{% endhighlight %}

Here we hook into the for-comprehension notation, by wrapping values
of type `T[A]` in an object with the methods we need (and `|` as an
additional bit of syntactic sugar), which methods just delegate to the
functions defined above. We arrange with an implicit conversion for
these wrappers to spring into existence when we need them.

<b>The bridge puzzle with the logic monad</b>

Now we can rewrite the solution in terms of the `Logic` trait:
{% highlight scala %}
class Bridge(Logic: Logic) {
  import Logic._
{% endhighlight %}

We pass an implementation of the logic monad in, then open it so the
implicit conversion is available (we can also use `T[A]` and the
`Logic` functions without qualification).

The `Person`, `times`, and `State` definitions are as before.

{% highlight scala %}
  private def chooseTwo(list: List[Person]): T[(Person,Person)] =
    for { p1 <- or(list); p2 <- or(list); if p1 < p2 }
    yield (p1, p2)
{% endhighlight %}

As we saw, we can write `chooseTwo` more straightforwardly using a
for-comprehension. In the previous version we punned on `list` as a
concrete list and as a choice among alternatives; here we convert one
to the other explicitly.

{% highlight scala %}
  private def next(state: State): T[State] = {
    if (state.lightOnLeft) {
      for {
        (p1, p2) <- chooseTwo(state.left)
        timeRemaining =
          state.timeRemaining - math.max(times(p1), times(p2))
        if timeRemaining >= 0
      } yield {
        val left =
          state.left.filterNot { p => p == p1 || p == p2 }
        State(left, false, timeRemaining)
      }
    } else { // ...
{% endhighlight %}

This is pretty much as before, except with for-comprehensions instead
of `foldLeft` and explicit consing. (You can easily figure out the
branch for the flashlight on the right.)

{% highlight scala %}
  private def tree(path: List[State]): T[List[State]] =
    unit(path) |
      (for {
         state <- next(path.head)
         path <- tree(state :: path)
       } yield path)

  def search(n: Int): List[List[State]] = {
    val start = List(State(Person.all, true, 60))
    val t =
      for { path <- tree(start); if path.head.left == Nil }
      yield path
    run(t, n)
  }
}
{% endhighlight %}

In `tree` we use `|` to adjoin the input path (previously we gave it
in the initial value of `foldLeft`). In `search` we need to actually
run the `Logic.T[A]` value rather than returning it, because it's an
abstract type and can't escape the module (see the Postscript for an
alternative); this is why the other methods must be `private`.

<b>Implementing the logic monad with lists</b>

We can recover the original solution by implementing `Logic` with lists:

{% highlight scala %}
object LogicList extends Logic {
  type T[A] = List[A]

  def fail[A] = Nil
  def unit[A](a: A) = a :: Nil
  def or[A](t1: List[A], t2: => List[A]) = t1 ::: t2
  def apply[A,B](t: List[A], f: A => B) = t.map(f)
  def bind[A,B](t: List[A], f: A => List[B]) = t.flatMap(f)
  def filter[A](t: List[A], p: A => Boolean) = t.filter(p)
  def split[A](t: List[A]) =
    t match {
      case Nil => None
      case h :: t => Some(h, t)
    }
}
{% endhighlight %}

A choice among alternatives is just a `List` of the alternatives, so
the semantics we sketched above are realized in a very direct way.

The downside to the `List` implementation is that we compute all the
alternatives, even if we only care about one of them. (In the bridge
problem any path to the final state is a satisfactory answer, but our
program computes all such paths, even if we pass an argument to
`search` requesting only one answer.) We might even want to solve
problems with an infinite number of solutions.

Next time we'll repair this downside by implementing the backtracking
monad from the paper by Kiselyov et al.

See the complete code
[here](https://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/scala-logic).

<b>Postscript: modules in Scala</b>

I got the idea of implementing the for-comprehension methods as an
implict wrapper from Edward Kmett's
[functorial](https://github.com/ekmett/functorial) library. It's nice
that `T[A]` remains completely abstract, and the for-comprehension
notation is just sugar. I also tried an implementation where `T[A]` is
bounded by a trait containing the methods:
{% highlight scala %}
trait Monadic[T[_], A] {
  def map[B](f: A => B): T[B]
  def filter(p: A => Boolean): T[A]
  def flatMap[B](f: A => T[B]): T[B]
  def withFilter(p: A => Boolean): T[A]

  def |(t: => T[A]): T[A]
  def split: Option[(A,T[A])]
}

trait Logic {
  type T[A] <: Monadic[T, A]
  // no Syntax class needed
{% endhighlight %}
This works too but the type system hackery is a bit ugly, and it
constrains implementations of `Logic` more than is necessary.

Another design choice is whether `T[A]` is an abstract type (as I have
it) or a type parameter of `Logic`:
{% highlight scala %}
trait Logic[T[_]] { L =>
  // no abstract type T[A] but otherwise as before
}
{% endhighlight %}
Neither alternative provides the expressivity of OCaml modules: with
abstract types, consumers of `Logic` cannot return values of `T[A]`
(as we saw above); with a type parameter, they can, but the
type is no longer abstract.

In OCaml we would write
{% highlight ocaml %}
module type Logic =
sig
  type 'a t

  val unit : 'a -> 'a t
  (* and so on *)
end

module Bridge(L : Logic) =
struct
  type state = ...
  val search : state list L.t
end
{% endhighlight %}
and get both the abstract type and the ability to return values of the type.
