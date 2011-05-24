object LogicStateSFKDefuncTailrec extends LogicState {
  type O[S,A] = Option[(S,A,T[S,A])]

  type T[S,A] = I

  sealed trait I

  case class Fail() extends I
  case class Unit(a: Any) extends I
  case class Or(t1: I, t2: () => I) extends I
  case class Bind(t: I, f: Any => I) extends I
  case class Apply(t: I, f: Any => Any) extends I
  case class Filter(t: I, p: Any => Boolean) extends I
  case class Unsplit(fk: I) extends I
  case object Get extends I
  case class Set(s: Any) extends I

  case class FKOr(t: () => I, s: Any, sk: I, fk: I) extends I
  case class FKSplit(r: O[Any,Any]) extends I

  case class SKBind(f: Any => I, sk: I) extends I
  case class SKApply(f: Any => Any, sk: I) extends I
  case class SKFilter(p: Any => Boolean, sk: I) extends I
  case class SKSplit(r: (Any, Any, I) => O[Any,Any]) extends I

  case object KReturn extends I
  case class KUnsplit(sk: I, fk: I, k: I) extends I

  def fail[S,A]: T[S,A] = Fail()
  def unit[S,A](a: A): T[S,A] = Unit(a)
  def or[S,A](t1: T[S,A], t2: => T[S,A]): T[S,A] = Or(t1, { () => t2 })
  def bind[S,A,B](t: T[S,A], f: A => T[S,B]): T[S,B] =
    Bind(t, f.asInstanceOf[Any => I])
  def apply[S,A,B](t: T[S,A], f: A => B): T[S,B] =
    Apply(t, f.asInstanceOf[Any => I])
  def filter[S,A](t: T[S,A], p: A => Boolean): T[S,A] =
    Filter(t, p.asInstanceOf[Any => Boolean])
  def get[S]: T[S,S] = Get
  def set[S](s: S): T[S,scala.Unit] = Set(s)

  def split[S,A](s: S, t: T[S,A]): O[S,A] = {

    def apply(i: I, s: Any, a: Any, r: O[Any,Any], sk: I, fk: I, k: I): O[Any,Any] =
      i match {
        case Fail() => apply(fk, null, null, null, null, null, k)
        case Unit(a) => apply(sk, s, a, null, null, fk, k)
        case Or(t1, t2) => apply(t1, s, null, null, sk, FKOr(t2, s, sk, fk), k)
        case Bind(t, f) => apply(t, s, null, null, SKBind(f, sk), fk, k)
        case Apply(t, f) => apply(t, s, null, null, SKApply(f, sk), fk, k)
        case Filter(t, p) => apply(t, s, null, null, SKFilter(p, sk), fk, k)
        case Unsplit(fk2) => apply(fk2, null, null, null, null, null, KUnsplit(sk, fk, k))
        case Get => apply(sk, s, s, null, null, fk, k)
        case Set(s) => apply(sk, s, (), null, null, fk, k)

        case FKOr(t, s, sk, fk) => apply(t(), s, null, null, sk, fk, k)
        case FKSplit(r) => apply(k, null, null, r, null, null, null)

        case SKBind(f, sk) => apply(f(a), s, null, null, sk, fk, k)
        case SKApply(f, sk) => apply(sk, s, f(a), null, null, fk, k)
        case SKFilter(p, sk) =>
          if (p(a))
            apply(sk, s, a, null, null, fk, k)
          else
            apply(fk, null, null, null, null, null, k)
        case SKSplit(rf) =>
          apply(k, null, null, rf(s, a, fk), null, null, null)

        case KReturn => r
        case KUnsplit(sk, fk, k) => {
          r match {
            case None => apply(fk, null, null, null, null, null, k)
            case Some((s, a, t)) =>
              apply(or(unit(a), t), s, null, null, sk, fk, k)
          }
        }
      }

    apply(t,
          s.asInstanceOf[Any],
          null,
          null,
          SKSplit((s, a, fk) => Some((s, a, Unsplit(fk)))),
          FKSplit(None),
          KReturn).asInstanceOf[O[S,A]]
  }
}
