import Term._

trait Test {
  val Scrolog: Scrolog
  import Scrolog._

  def member[A](x: Term[A], l: Term[List[A]]): P = {
    val hd = Evar[A]("hd"); val tl = Evar[List[A]]("tl")
    ConsTerm(hd, tl) =:= l &
    (x =:= hd | member(x, tl))
  }

  sealed trait Nat
  case object Z extends Nat
  case class S(n: Nat) extends Nat

  case object ZTerm extends Term[Nat] {
    def unify(e: Env, t: Term[Nat]) =
      t match {
        case ZTerm => Some(e)
        case _: VarTerm[_] => t.unify(e, this)
        case _ => None
      }

    def occurs[A](v: Evar[A]) = false
    def subst(e: Env) = this
    def ground(e: Env) = Z

    override def toString = { Z.toString }
  }

  case class STerm(n: Term[Nat]) extends Term[Nat] {
    def unify(e: Env, t: Term[Nat]) =
      t match {
        case STerm(n2) => n.unify(e, n2)
        case _: VarTerm[_] => t.unify(e, this)
        case _ => None
      }

    def occurs[A](v: Evar[A]) = n.occurs(v)
    def subst(e: Env) = STerm(n.subst(e))
    def ground(e: Env) = S(n.ground(e))

    override def toString = { "S(" + n.toString + ")" }
  }

  implicit def nat2Term(n: Nat): Term[Nat] =
    n match {
      case Z => ZTerm
      case S(n) => STerm(nat2Term(n))
    }

  def sum(m: Term[Nat], n: Term[Nat], p: Term[Nat]): P = {
    val m2 = Evar[Nat]("m2"); val p2 = Evar[Nat]("p2")
    (m =:= Z & n =:= p) |
    (m =:= STerm(m2) & p =:= STerm(p2) & sum(m2, n, p2))
  }
}
