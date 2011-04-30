object LogicSFKDefuncTailrec extends Logic {
  type O[A] = Option[(A,T[A])]

  sealed trait T[A]
  case class Fail[A]() extends T[A]
  case class Unit[A](a: A) extends T[A]
  case class Or[A](t1: T[A], t2: () => T[A]) extends T[A]
  case class Bind[A,B](t: T[A], f: A => T[B]) extends T[B]
  case class Apply[A,B](t: T[A], f: A => B) extends T[B]
  case class Filter[A](t: T[A], p: A => Boolean) extends T[A]
  case class Unsplit[A](fk: FK) extends T[A]

  sealed trait FK
  case class FKOr(t: () => T[Any], sk: SK, fk: FK) extends FK
  case class FKSplit(r: O[Any]) extends FK

  sealed trait SK
  case class SKBind(f: Any => T[Any], sk: SK) extends SK
  case class SKApply(f: Any => Any, sk: SK) extends SK
  case class SKFilter(p: Any => Boolean, sk: SK) extends SK
  case class SKSplit(r: (Any, FK) => O[Any]) extends SK

  sealed trait K
  case object KReturn extends K
  case class KUnsplit(sk: SK, fk: FK, k: K) extends K

  def fail[A] = Fail()
  def unit[A](a: A) = Unit(a)
  def or[A](t1: T[A], t2: => T[A]) = Or(t1, { () => t2 })
  def bind[A,B](t: T[A], f: A => T[B]) = Bind(t, f)
  def apply[A,B](t: T[A], f: A => B) = Apply(t, f)
  def filter[A](t: T[A], p: A => Boolean) = Filter(t, p)

  def split[A](t2: T[A]): O[A] = {
    var app = 0
    var t = t2.asInstanceOf[T[Any]]
    var a: Any = null
    var r: O[Any] = null
    var sk: SK = SKSplit((a, fk) => Some((a, Unsplit(fk))))
    var fk: FK = FKSplit(None)
    var k: K = KReturn

    while (true) {
      app match {
        case 0 => // applyT
          t match {
            case Fail() => app = 1
            case Unit(a2) => { a = a2; app = 2 }
            case Or(t1, t2) => { t = t1; fk = FKOr(t2, sk, fk) }
            case Bind(t2, f) => { t = t2; sk = SKBind(f, sk) }
            case Apply(t2, f) => { t = t2; sk = SKApply(f, sk) }
            case Filter(t2, p) => { t = t2; sk = SKFilter(p, sk) }
            case Unsplit(fk2) => {
              app = 1; k = KUnsplit(sk, fk, k); fk = fk2
            }
          }

        case 1 => // applyFK
          fk match {
            case FKOr(t2, sk2, fk2) => {
              app = 0; t = t2(); sk = sk2; fk = fk2
            }
            case FKSplit(r2) => { app = 3; r = r2 }
          }

        case 2 => // applySK
          sk match {
            case SKBind(f, sk2) => { app = 0; t = f(a); sk = sk2 }
            case SKApply(f, sk2) => { sk = sk2; a = f(a) }
            case SKFilter(p, sk2) =>
              if (p(a)) sk = sk2 else app = 1
            case SKSplit(rf) => { app = 3; r = rf(a, fk) }
          }

        case 3 => // applyK
          k match {
            case KReturn => return r.asInstanceOf[O[A]]
            case KUnsplit(sk2, fk2, k2) =>
              r match {
                case None => { app = 1; fk = fk2; k = k2 }
                case Some((a2, t2)) => {
                  app = 0; t = or(unit(a2), t2); sk = sk2
                  fk = fk2; k = k2
                }
              }
          }
      }
    }
    throw new Exception("not reached")
  }
}
