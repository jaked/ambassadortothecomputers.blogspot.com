import scala.collection.immutable.{Map,HashMap}

class Evar[A](val name: String)
object Evar { def apply[A](name: String) = new Evar[A](name) }

trait Term[A] {
  // invariant: on call to unify, this and t have e substituted
  def unify(e: Env, t: Term[A]): Option[Env]

  def occurs[B](v: Evar[B]): Boolean
  def subst(e: Env): Term[A]
  def ground: A
}

class Env(m: Map[Evar[Any],Term[Any]]) {
  def apply[A](v: Evar[A]) =
    m(v.asInstanceOf[Evar[Any]]).asInstanceOf[Term[A]]
  def get[A](v: Evar[A]): Option[Term[A]] =
    m.get(v.asInstanceOf[Evar[Any]]).asInstanceOf[Option[Term[A]]]
  def updated[A](v: Evar[A], t: Term[A]): Env = {
    val v2 = v.asInstanceOf[Evar[Any]]
    val t2 = t.asInstanceOf[Term[Any]]
    val e2 = Env(Map(v2 -> t2))
    val m2 = m.mapValues(_.subst(e2))
    Env(m2.updated(v2, t2))
  }

  override def toString = {
    "{ " + m.map({ case (k, v) => k.name + " = " + v.toString }).mkString(", ") + " }"
  }
}
object Env {
  def apply(m: Map[Evar[Any],Term[Any]]) = new Env(m)
  def empty = new Env(HashMap())
}

case class VarTerm[A](v: Evar[A]) extends Term[A] {
  def unify(e: Env, t: Term[A]) =
    t match {
      case VarTerm(v2) if (v2 == v) => Some(e)
      case _ =>
        if (t.occurs(v)) None
        else Some(e.updated(v, t))
    }

  def occurs[B](v2: Evar[B]) = v2 == v

  def subst(e: Env) =
    e.get(v) match {
      case Some(t) => t
      case None => this
    }

  def ground =
    throw new IllegalArgumentException("not ground")

  override def toString = { v.name  }
}

case class LitTerm[A](a: A) extends Term[A] {
  def unify(e: Env, t: Term[A]) =
    t match {
      case LitTerm(a2) => if (a == a2) Some(e) else None
      case _: VarTerm[_] => t.unify(e, this)
      case _ => None
    }

  def occurs[B](v: Evar[B]) = false
  def subst(e: Env) = this
  def ground = a

  override def toString = { a.toString }
}

case class Tuple2Term[A,B](_1: Term[A], _2: Term[B])
  extends Term[(A,B)]
{
  def unify(e: Env, t: Term[(A,B)]) =
    t match {
      case Tuple2Term(_1_, _2_) =>
        for {
          e1 <- _1.unify(e, _1_)
          e2 <- _2.subst(e1).unify(e1, _2_.subst(e1))
        } yield e2
      case _: VarTerm[_] => t.unify(e, this)
      case _ => None
    }

  def occurs[C](v: Evar[C]) = _1.occurs(v) || _2.occurs(v)
  def subst(e: Env) = Tuple2Term(_1.subst(e), _2.subst(e))
  def ground = (_1.ground, _2.ground)

  override def toString = { (_1, _2).toString }
}

case class NilTerm[A]() extends Term[List[A]] {
  def unify(e: Env, t: Term[List[A]]) =
    t match {
      case NilTerm() => Some(e)
      case _: VarTerm[_] => t.unify(e, this)
      case _ => None
    }

  def occurs[B](v: Evar[B]) = false
  def subst(e: Env) = this
  def ground = Nil

  override def toString = { Nil.toString }
}

case class ConsTerm[A](hd: Term[A], tl: Term[List[A]])
  extends Term[List[A]]
{
  def unify(e: Env, t: Term[List[A]]) =
    t match {
      case ConsTerm(hd2, tl2) =>
        for {
          e1 <- hd.unify(e, hd2)
          e2 <- tl.subst(e1).unify(e1, tl2.subst(e1))
        } yield e2
      case _: VarTerm[_] => t.unify(e, this)
      case _ => None
    }

  def occurs[C](v: Evar[C]) = hd.occurs(v) || tl.occurs(v)
  def subst(e: Env) = ConsTerm(hd.subst(e), tl.subst(e))
  def ground = hd.ground :: tl.ground

  override def toString = { hd.toString + " :: " + tl.toString }
}

object Term {
  implicit def var2Term[A](v: Evar[A]): Term[A] = VarTerm(v)
  //implicit def lit2term[A](a: A): Term[A] = LitTerm(a)
  implicit def int2Term(a: Int): Term[Int] = LitTerm(a)
  implicit def tuple2Term[A,B](ab: Tuple2[Term[A],Term[B]]): Term[(A,B)] =
    Tuple2Term(ab._1, ab._2)
  implicit def list2Term[A](l: List[Term[A]]): Term[List[A]] =
    l match {
      case Nil => NilTerm[A]
      case hd :: tl => ConsTerm(hd, list2Term(tl))
    }
}
