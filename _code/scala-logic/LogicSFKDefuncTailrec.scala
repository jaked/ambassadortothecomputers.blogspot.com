object LogicSFKDefuncTailrec extends Logic {
  type O[A] = Option[(A,T[A])]

  type T[A] = I

  sealed trait I
  case class Fail() extends I
  case class Unit(a: Any) extends I
  case class Or(t1: I, t2: () => I) extends I
  case class Bind(t: I, f: Any => I) extends I
  case class Apply(t: I, f: Any => Any) extends I
  case class Filter(t: I, p: Any => Boolean) extends I
  case class Unsplit(fk: I) extends I

  case class FKOr(t: () => I, sk: I, fk: I) extends I
  case class FKSplit(r: O[Any]) extends I

  case class SKBind(f: Any => I, sk: I) extends I
  case class SKApply(f: Any => Any, sk: I) extends I
  case class SKFilter(p: Any => Boolean, sk: I) extends I
  case class SKSplit(r: (Any, I) => O[Any]) extends I

  case object KReturn extends I
  case class KUnsplit(sk: I, fk: I, k: I) extends I

  def fail[A]: T[A] = Fail()
  def unit[A](a: A): T[A] = Unit(a)
  def or[A](t1: T[A], t2: => T[A]): T[A] = Or(t1, { () => t2 })
  def bind[A,B](t: T[A], f: A => T[B]): T[B] =
    Bind(t, f.asInstanceOf[Any => I])
  def apply[A,B](t: T[A], f: A => B): T[B] =
    Apply(t, f.asInstanceOf[Any => I])
  def filter[A](t: T[A], p: A => Boolean): T[A] =
    Filter(t, p.asInstanceOf[Any => Boolean])

  def split[A](t: T[A]): O[A] = {
    def apply(i: I, a: Any, r: O[Any], sk: I, fk: I, k: I): O[Any] =
      i match {
        case Fail() => apply(fk, null, null, null, null, k)
        case Unit(a) => apply(sk, a, null, null, fk, k)
        case Or(t1, t2) =>
          apply(t1, null, null, sk, FKOr(t2, sk, fk), k)
        case Bind(t, f) =>
          apply(t, null, null, SKBind(f, sk), fk, k)
        case Apply(t, f) =>
          apply(t, null, null, SKApply(f, sk), fk, k)
        case Filter(t, p) =>
          apply(t, null, null, SKFilter(p, sk), fk, k)
        case Unsplit(fk2) =>
          apply(fk2, null, null, null, null, KUnsplit(sk, fk, k))

        case FKOr(t, sk, fk) => apply(t(), null, null, sk, fk, k)
        case FKSplit(r) => apply(k, null, r, null, null, null)

        case SKBind(f, sk) => apply(f(a), null, null, sk, fk, k)
        case SKApply(f, sk) => apply(sk, f(a), null, null, fk, k)
        case SKFilter(p, sk) =>
          if (p(a))
            apply(sk, a, null, null, fk, k)
          else
            apply(fk, null, null, null, null, k)
        case SKSplit(rf) =>
          apply(k, null, rf(a, fk), null, null, null)

        case KReturn => r
        case KUnsplit(sk, fk, k) => {
          r match {
            case None => apply(fk, null, null, null, null, k)
            case Some((a, t)) =>
              apply(or(unit(a), t), null, null, sk, fk, k)
          }
        }
      }

    apply(t,
          null,
          null,
          SKSplit((a, fk) => Some((a, Unsplit(fk)))),
          FKSplit(None),
          KReturn).asInstanceOf[O[A]]
  }
}
