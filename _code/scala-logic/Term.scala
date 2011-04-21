import scala.collection.immutable.HashMap

class Evar[A](val name: String)
object Evar { def apply[A](name: String) = new Evar[A](name) }

class Env(m: HashMap[Evar[Any],Term[Any]]) {
  def apply[A](v: Evar[A]) =
    m(v.asInstanceOf[Evar[Any]]).asInstanceOf[Term[A]]
  def get[A](v: Evar[A]): Option[Term[A]] =
    m.get(v.asInstanceOf[Evar[Any]]).asInstanceOf[Option[Term[A]]]
  def updated[A](v: Evar[A], t: Term[A]) =
    Env(m.updated(v.asInstanceOf[Evar[Any]], t.asInstanceOf[Term[Any]]))

  override def toString = {
    "{ " + m.map({ case (k, v) => k.name + " = " + v.toString }).mkString(", ") + " }"
  }
}
object Env {
  def apply(m: HashMap[Evar[Any],Term[Any]]) = new Env(m)
  def empty = new Env(HashMap())
}

trait Term[A] {
  def unify(e: Env, t: Term[A]): Option[Env]
  def occurs[B](e: Env, v: Evar[B]): Boolean
  def subst(e: Env): Term[A]
  def ground(e: Env): A

  import LogicStateSKE._
  def =!=(t2: Term[A]): T[Env, Unit] =
    for {
      env <- get
      env2 <-
      (unify(env, t2) match {
        case None => fail[Env,Unit]
        case Some(e) => set(e)
      })
    } yield env2
}

case class VarTerm[A](v: Evar[A]) extends Term[A] {
  def unify(e: Env, t: Term[A]) =
    e.get(v) match {
      case Some(t2) => t2.unify(e, t)
      case None =>
        t match {
          case VarTerm(v2) if (v2 == v) => Some(e)
          case _ =>
            if (t.occurs(e, v)) None
            else Some(e.updated(v, t))
        }
    }

  def occurs[B](e: Env, v2: Evar[B]) =
    e.get(v) match {
      case Some(t) => t.occurs(e, v2)
      case None => v2 == v
    }

  def subst(e: Env) =
    e.get(v) match {
      case Some(t) => t.subst(e)
      case None => this
    }

  def ground(e: Env) =
    e.get(v) match {
      case Some(t) => t.ground(e)
      case None => throw new IllegalArgumentException("not ground")
    }

  override def toString = { v.name  }
}

case class LitTerm[A](a: A) extends Term[A] {
  def unify(e: Env, t: Term[A]) =
    t match {
      case LitTerm(a2) => if (a == a2) Some(e) else None
      case _: VarTerm[_] => t.unify(e, this)
      case _ => t.unify(e, this)
    }

  def occurs[B](e: Env, v2: Evar[B]) = false
  def subst(e: Env) = this
  def ground(e: Env) = a

  override def toString = { a.toString }
}

case class Tuple2Term[A,B](_1: Term[A], _2: Term[B]) extends Term[(A,B)] {
  def unify(e: Env, t: Term[(A,B)]) =
    t match {
      case Tuple2Term(_1_, _2_) =>
        for (e1 <- _1.unify(e, _1_); e2 <- _2.unify(e1, _2_)) yield e2
      case _: VarTerm[_] => t.unify(e, this)
      case _ => None
    }

  def occurs[C](e: Env, v: Evar[C]) = _1.occurs(e, v) || _2.occurs(e, v)
  def subst(e: Env) = Tuple2Term(_1.subst(e), _2.subst(e))
  def ground(e: Env) = (_1.ground(e), _2.ground(e))

  override def toString = { (_1, _2).toString }
}

case class NilTerm[A]() extends Term[List[A]] {
  def unify(e: Env, t: Term[List[A]]) =
    t match {
      case NilTerm() => Some(e)
      case _: VarTerm[_] => t.unify(e, this)
      case _ => None
    }

  def occurs[B](e: Env, v: Evar[B]) = false
  def subst(e: Env) = this
  def ground(e: Env) = Nil

  override def toString = { Nil.toString }
}

case class ConsTerm[A](hd: Term[A], tl: Term[List[A]]) extends Term[List[A]] {
  def unify(e: Env, t: Term[List[A]]) =
    t match {
      case ConsTerm(hd2, tl2) =>
        for (e1 <- hd.unify(e, hd2); e2 <- tl.unify(e1, tl2)) yield e2
      case _: VarTerm[_] => t.unify(e, this)
      case _ => None
    }

  def occurs[C](e: Env, v: Evar[C]) = hd.occurs(e, v) || tl.occurs(e, v)
  def subst(e: Env) = ConsTerm(hd.subst(e), tl.subst(e))
  def ground(e: Env) = hd.ground(e) :: tl.ground(e)

  override def toString = { hd.toString + " :: " + tl.toString }
}

object Term {
  implicit def var2term[A](v: Evar[A]): VarTerm[A] = VarTerm(v)
  //implicit def lit2term[A](a: A): LitTerm[A] = LitTerm(a)
  implicit def int2term(a: Int): LitTerm[Int] = LitTerm(a)
  implicit def tuple2term[A,B](ab: Tuple2[Term[A],Term[B]]): Tuple2Term[A,B] =
    Tuple2Term(ab._1, ab._2)
  implicit def list2term[A](l: List[Term[A]]): Term[List[A]] =
    l match {
      case Nil => NilTerm[A]
      case hd :: tl => ConsTerm(hd, list2term(tl))
    }
}

object Run {
  import LogicStateSKE._

  def run[A](t: T[Env,Unit], n: Int, tm: Term[A]): List[Term[A]] =
    LogicStateSKE.run(Env.empty, t, n).map({ case (e, _) => tm.subst(e) })
}
