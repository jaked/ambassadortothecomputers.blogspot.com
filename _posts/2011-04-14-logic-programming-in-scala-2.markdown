---
layout: blogspot
title: Logic programming in Scala
---

<b>Implementing the logic monad with success/failure continuations</b>

{% highlight scala %}
object LogicSFK extends Logic {
  // type FK[R] = => R
  type SK[A,R] = (A, => R) => R

  trait T[A] { def apply[R](sk: SK[A,R], fk: => R): R }

  def fail[A] =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: => R) = fk
    }

  def unit[A](a: A) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: => R) = sk(a, fk)
    }

  def or[A](t1: T[A], t2: => T[A]) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: => R) = t1(sk, t2(sk, fk))
    }

  def bind[A,B](t: T[A], f: A => T[B]) =
    new T[B] {
      def apply[R](sk: SK[B,R], fk: => R) =
        t(({ (a, fk) => f(a)(sk, fk) }: SK[A,R]), fk)
    }

  def apply[A,B](t: T[A], f: A => B) =
    new T[B] {
      def apply[R](sk: SK[B,R], fk: => R) =
        t(({ (a, fk) => sk(f(a), fk) }: SK[A,R]), fk)
    }

  def filter[A](t: T[A], p: A => Boolean) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: => R) =
        t(({ (a, fk) => if (p(a)) sk(a, fk) else fk }: SK[A,R]), fk)
    }

  def split[A](t: T[A]) = {
    def unsplit(r: Option[(A,T[A])]): T[A] =
      r match {
        case None => fail
        case Some((a, t)) => or(unit(a), t)
      }
    def unit[A](a: => A) =
      new T[A] {
        def apply[R](sk: SK[A,R], fk: => R) = sk(a, fk)
      }
    def sk : SK[A,Option[(A,T[A])]] =
      { (a, fk) => Some(a, bind(unit(fk), unsplit)) }
    t(sk, None)
  }
}
{% endhighlight %}

<b>Threading state through the logic monad</b>

<b>Unification</b>

<b>Logic programming</b>
