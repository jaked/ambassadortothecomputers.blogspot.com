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

<b>Backtracking with exceptions</b>

{% highlight scala %}
case object Fail extends Exception
case object Finish extends Exception

object LogicSKE extends Logic {
  type T[A] = (A => Unit) => Unit

  def fail[A] = { sk => throw Fail }

  def unit[A](a: A) = { sk => sk(a) }

  def or[A](t1: T[A], t2: => T[A]) =
    { sk =>
      try { t1(sk) }
      catch { case Fail => t2(sk) }
    }

  def bind[A,B](t: T[A], f: A => T[B]) =
    { sk => t(a => f(a)(sk)) }

  def apply[A,B](t: T[A], f: A => B) =
    { sk => t(a => sk(f(a))) }

  def filter[A](t: T[A], p: A => Boolean) =
    { sk =>
      t(a => if (p(a)) sk(a) else throw Fail)
    }

  def split[A](t: T[A]) = throw new Exception("unimplemented")

  override def run[A](t: T[A], n: Int): List[A] = {
    if (n <= 0) return Nil
    val lb = new scala.collection.mutable.ListBuffer[A]
    def sk(a: A) = {
      lb += a
      throw (if (lb.size < n) Fail else Finish)
    }
    try {
      t(sk)
      throw Finish // for the typechecker, not reached
    }
    catch { case Fail | Finish => lb.result }
  }
}
{% endhighlight %}

<b>Unification</b>

<b>Threading state through the logic monad</b>

{% highlight scala %}
trait LogicState { L =>
  type T[S,A]

  def run[S,A](s: S, t: T[S,A], n: Int): List[(S,A)]
  // def split[S,A](s: S, t: T[S,A]): Option[(S,A,T[S,A])]

  // as before with extra S parameter
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

  def split[S,A](s: S, t: T[S,A]) =
    throw new Exception("unimplemented")

  override def run[S,A](s: S, t: T[S,A], n: Int): List[(S,A)] = {
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

<b>Logic programming</b>
