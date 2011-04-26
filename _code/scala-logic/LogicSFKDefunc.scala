object LogicSFKDefunc extends Logic {
  sealed trait T[A]
  case class Fail[A]() extends T[A]
  case class Unit[A](a: A) extends T[A]
  case class Or[A](t1: T[A], t2: () => T[A]) extends T[A]
  case class Bind[A,B](t: T[A], f: A => T[B]) extends T[B]
  case class Apply[A,B](t: T[A], f: A => B) extends T[B]
  case class Filter[A](t: T[A], p: A => Boolean) extends T[A]

  sealed trait FK[R]
  case class FKOr[A,R](t: () => T[A], sk: SK[A,R], fk: FK[R]) extends FK[R]
  case class FKSplit[R](r: R) extends FK[R]

  sealed trait SK[A,R]
  case class SKBind[A,B,R](f: A => T[B], sk: SK[B,R]) extends SK[A,R]
  case class SKApply[A,B,R](f: A => B, sk: SK[B,R]) extends SK[A,R]
  case class SKFilter[A,R](p: A => Boolean, sk: SK[A,R]) extends SK[A,R]
  case class SKSplit[A,R](r: (A, FK[R]) => R) extends SK[A,R]

  def fail[A] = Fail()
  def unit[A](a: A) = Unit(a)
  def or[A](t1: T[A], t2: => T[A]) = Or(t1, { () => t2 })
  def bind[A,B](t: T[A], f: A => T[B]) = Bind(t, f)
  def apply[A,B](t: T[A], f: A => B) = Apply(t, f)
  def filter[A](t: T[A], p: A => Boolean) = Filter(t, p)

  def split[A](t: T[A]) = {
    def unsplit[A](fk: FK[Option[(A, T[A])]]): T[A] =
      applyFK(fk) match {
        case None => fail
        case Some((a, t)) => or(unit(a), t)
      }

    def applyT[A,R](t: T[A], sk: SK[A,R], fk: FK[R]): R =
      t match {
        case Fail() => applyFK(fk)
        case Unit(a) => applySK(sk, a, fk)
        case Or(t1, t2) => applyT(t1, sk, FKOr(t2, sk, fk))
        case Bind(t, f) => applyT(t, SKBind(f, sk), fk)
        case Apply(t, f) => applyT(t, SKApply(f, sk), fk)
        case Filter(t, p) => applyT(t, SKFilter(p, sk), fk)
      }

    def applyFK[R](fk: FK[R]): R =
      fk match {
        case FKOr(t, sk, fk) => applyT(t(), sk, fk)
        case FKSplit(r) => r
      }

    def applySK[A,R](sk: SK[A,R], a: A, fk: FK[R]): R =
      sk match {
        case SKBind(f, sk) => applyT(f(a), sk, fk)
        case SKApply(f, sk) => applySK(sk, f(a), fk)
        case SKFilter(p, sk) => if (p(a)) applySK(sk, a, fk) else applyFK(fk)
        case SKSplit(r) => r(a, fk)
      }

    applyT[A,Option[(A,T[A])]](
      t,
      SKSplit((a, fk) => Some((a, bind(unit(fk), unsplit)))),
      FKSplit(None))
  }
}
