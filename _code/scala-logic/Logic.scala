trait Logic { L =>
  type T[A]

  def fail[A]: T[A]
  def unit[A](a: A): T[A]
  def or[A](t1: T[A], t2: => T[A]): T[A]
  def apply[A,B](t: T[A], f: A => B): T[B]
  def bind[A,B](t: T[A], f: A => T[B]): T[B]
  def filter[A](t: T[A], p: A => Boolean): T[A]
  def split[A](t: T[A]): Option[(A,T[A])]

  def or[A](as: List[A]): T[A] =
    as.foldRight(fail[A])((a, t) => or(unit(a), t))

  def run[A](t: T[A], n: Int): List[A] =
    if (n <= 0) Nil else
      split(t) match {
        case None => Nil
        case Some((a, t)) => a :: run(t, n - 1)
      }

  case class Syntax[A](t: T[A]) {
    def map[B](f: A => B): T[B] = L.apply(t, f)
    def filter(p: A => Boolean): T[A] = L.filter(t, p)
    def flatMap[B](f: A => T[B]): T[B] = L.bind(t, f)
    def withFilter(p: A => Boolean): T[A] = L.filter(t, p)

    def |(t2: => T[A]): T[A] = L.or(t, t2)
  }

  implicit def syntax[A](t: T[A]) = Syntax(t)
}

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
