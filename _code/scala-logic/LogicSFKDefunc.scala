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

  def fail[A] = Fail()
  def unit[A](a: A) = Unit(a)
  def or[A](t1: T[A], t2: => T[A]) = Or(t1, { () => t2 })
  def bind[A,B](t: T[A], f: A => T[B]) = Bind(t, f)
  def apply[A,B](t: T[A], f: A => B) = Apply(t, f)
  def filter[A](t: T[A], p: A => Boolean) = Filter(t, p)

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

    applyT[A,O[A],O[A]](
      t,
      SKSplit((a, fk) => Some((a, Unsplit(fk)))),
      FKSplit(None),
      KReturn())
  }
}
