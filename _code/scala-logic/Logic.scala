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

  def run[A](t: T[A], n: Int): List[A] = {
    def runAcc(t: T[A], n: Int, acc: List[A]): List[A] =
      if (n <= 0) acc.reverse else
        split(t) match {
          case None => Nil
          case Some((a, t)) => runAcc(t, n - 1, a :: acc)
        }
    runAcc(t, n, Nil)
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
  type FK[R] = () => R
  type SK[A,R] = (A, FK[R]) => R

  trait T[A] { def apply[R](sk: SK[A,R], fk: FK[R]): R }

  def fail[A] =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: FK[R]) = fk()
    }

  def unit[A](a: A) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: FK[R]) = sk(a, fk)
    }

  def or[A](t1: T[A], t2: => T[A]) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: FK[R]) = t1(sk, { () => t2(sk, fk) })
    }

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

  def filter[A](t: T[A], p: A => Boolean) =
    new T[A] {
      def apply[R](sk: SK[A,R], fk: FK[R]) =
        t(({ (a, fk) => if (p(a)) sk(a, fk) else fk() }: SK[A,R]), fk)
    }

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

object LogicSKE extends Logic {
  case object Fail extends Exception

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

  case object Finish extends Exception

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
      throw new Exception("not reached")
    }
    catch { case Fail | Finish => lb.result }
  }
}

object LogicSKE2 extends Logic {
  case object Fail extends Exception
  class Succeed[A](val a: A, var s: List[() => Unit]) extends Exception

  type T[A] = (A => Unit) => Unit

  def fail[A] = { sk => throw Fail }

  def unit[A](a: A) = { sk => sk(a) }

  def or[A](t1: T[A], t2: => T[A]) =
    { sk =>
      try { t1(sk) }
      catch {
        case Fail => t2(sk)
        case e: Succeed[_] => {
          e.s = { () => t2(sk) } :: e.s
          throw e
        }
      }
    }

  def bind[A,B](t: T[A], f: A => T[B]) =
    { sk => t(a => f(a)(sk)) }

  def apply[A,B](t: T[A], f: A => B) =
    { sk => t(a => sk(f(a))) }

  def filter[A](t: T[A], p: A => Boolean) =
    { sk =>
      t(a => if (p(a)) sk(a) else throw Fail)
    }

  def split[A](t: T[A]) = {
    def restore(s: List[() => Unit]) {
      s match {
        case Nil => throw Fail
        case cp :: s =>
          try { restore(s) }
          catch {
            case Fail => cp()
            case e: Succeed[_] => {
              e.s = cp :: e.s
              throw e
            }
          }
      }
    }
    def unsplit[A](s: List[() => Unit]): T[A] =
      try { restore(s); throw new Exception("not reached") }
      catch {
        case Fail => fail
        case e: Succeed[A] => or(unit(e.a), bind(unit(e.s), unsplit))
      }

    try { t(a => throw new Succeed(a, Nil)); throw new Exception("not reached") }
    catch {
      case Fail => None
      case e: Succeed[A] => Some((e.a, bind(unit(e.s), unsplit)))
    }
  }
}
