trait LogicStateT extends LogicState {
  val Logic: Logic

  type T[S,A] = S => Logic.T[(S, A)]

  def fail[S,A] = { s: S => Logic.fail }
  def unit[S,A](a: A) = { s: S => Logic.unit((s, a)) }

  def or[S,A](t1: T[S,A], t2: => T[S,A]) =
    { s: S => Logic.or(t1(s), t2(s)) }

  def bind[S,A,B](t: T[S,A], f: A => T[S,B]) = {
    val f2: ((S,A)) => Logic.T[(S,B)] = { case (s, a) => f(a)(s) }
    { s: S => Logic.bind(t(s), f2) }
  }

  def apply[S,A,B](t: T[S,A], f: A => B) = {
    val f2: ((S,A)) => ((S,B)) = { case (s, a) => (s, f(a)) }
    { s: S => Logic.apply(t(s), f2) }
  }

  def filter[S,A](t: T[S,A], p: A => Boolean) =
    { s: S => Logic.filter(t(s), { sa: (S,A) => p(sa._2) }) }

  def split[S,A](s: S, t: T[S,A]) = {
    Logic.split(t(s)) match {
      case None => None
      case Some(((s, a), t)) => Some((s, a, { _ => t }))
    }
  }

  def get[S] = { s: S => Logic.unit((s,s)) }
  def set[S](s: S) = { _: S => Logic.unit((s,())) }
}
