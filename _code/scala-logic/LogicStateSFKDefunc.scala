object LogicStateSFKDefunc extends LogicState {
  type O[S,A] = Option[(S,A,T[S,A])]

  sealed trait T[S,A]
  case class Fail[S,A]() extends T[S,A]
  case class Unit[S,A](a: A) extends T[S,A]
  case class Or[S,A](t1: T[S,A], t2: () => T[S,A]) extends T[S,A]
  case class Bind[S,A,B](t: T[S,A], f: A => T[S,B]) extends T[S,B]
  case class Apply[S,A,B](t: T[S,A], f: A => B) extends T[S,B]
  case class Filter[S,A](t: T[S,A], p: A => Boolean) extends T[S,A]
  case class Unsplit[S,A](fk: FK[O[S,A]]) extends T[S,A]
  case class Get[S]() extends T[S,S]
  case class Set[S](s: S) extends T[S,scala.Unit]

  def fail[S,A] = Fail()
  def unit[S,A](a: A) = Unit(a)
  def or[S,A](t1: T[S,A], t2: => T[S,A]) = Or(t1, { () => t2 })
  def bind[S,A,B](t: T[S,A], f: A => T[S,B]) = Bind(t, f)
  def apply[S,A,B](t: T[S,A], f: A => B) = Apply(t, f)
  def filter[S,A](t: T[S,A], p: A => Boolean) = Filter(t, p)
  def get[S] = Get()
  def set[S](s: S) = Set(s)

  sealed trait FK[R]
  case class FKOr[S,A,R](t: () => T[S,A], s: S, sk: SK[S,A,R], fk: FK[R]) extends FK[R]
  case class FKSplit[R](r: R) extends FK[R]

  sealed trait SK[S,A,R]
  case class SKBind[S,A,B,R](f: A => T[S,B], sk: SK[S,B,R]) extends SK[S,A,R]
  case class SKApply[S,A,B,R](f: A => B, sk: SK[S,B,R]) extends SK[S,A,R]
  case class SKFilter[S,A,R](p: A => Boolean, sk: SK[S,A,R]) extends SK[S,A,R]
  case class SKSplit[S,A,R](r: (S, A, FK[R]) => R) extends SK[S,A,R]

  sealed trait K[R,R2]
  case class KReturn[R]() extends K[R,R]
  case class KUnsplit[S,A,R,R2](sk: SK[S,A,R], fk:FK[R], k: K[R,R2]) extends K[O[S,A],R2]

  def split[S,A](s: S, t: T[S,A]) = {

    def applyT[A,R,R2](t: T[S,A], s: S, sk: SK[S,A,R], fk: FK[R], k: K[R,R2]): R2 =
      t match {
        case Fail() => applyFK(fk, k)
        case Unit(a) => applySK(sk, s, a, fk, k)
        case Or(t1, t2) => applyT(t1, s, sk, FKOr(t2, s, sk, fk), k)
        case Bind(t, f) => applyT(t, s, SKBind(f, sk), fk, k)
        case Apply(t, f) => applyT(t, s, SKApply(f, sk), fk, k)
        case Filter(t, p) => applyT(t, s, SKFilter(p, sk), fk, k)
        case Unsplit(fk2) => applyFK(fk2, KUnsplit(sk, fk, k))
        case Get() => applySK(sk.asInstanceOf[SK[S,S,R]], s, s, fk, k)
        case Set(s) => applySK(sk.asInstanceOf[SK[S,scala.Unit,R]], s, (), fk, k)
      }

    def applyFK[R,R2](fk: FK[R], k: K[R,R2]): R2 =
      fk match {
        case FKOr(t, s, sk, fk) =>
          applyT(t().asInstanceOf[T[S,Any]],
                 s.asInstanceOf[S],
                 sk.asInstanceOf[SK[S,Any,R]],
                 fk, k)
        case FKSplit(r) => applyK(k, r)
      }

    def applySK[A,R,R2](sk: SK[S,A,R], s: S, a: A, fk: FK[R], k: K[R,R2]): R2 =
      sk match {
        case SKBind(f, sk) => applyT(f(a), s, sk, fk, k)
        case SKApply(f, sk) => applySK(sk, s, f(a), fk, k)
        case SKFilter(p, sk) =>
          if (p(a)) applySK(sk, s, a, fk, k) else applyFK(fk, k)
        case SKSplit(rf) => applyK(k, rf(s, a, fk))
      }

    def applyK[R,R2](k: K[R,R2], r: R): R2 =
      k match {
        case KReturn() => r.asInstanceOf[R2]
        case KUnsplit(sk, fk, k) => {
          r match {
            case None => applyFK(fk, k)
            case Some((s, a, t)) =>
              applyT(or(unit(a), t.asInstanceOf[T[S,Any]]),
                     s.asInstanceOf[S],
                     sk.asInstanceOf[SK[S,Any,Any]],
                     fk, k)
          }
        }
      }

    applyT[A,O[S,A],O[S,A]](
      t,
      s,
      SKSplit((s, a, fk) => Some((s, a, Unsplit(fk)))),
      FKSplit(None),
      KReturn())
  }
}
