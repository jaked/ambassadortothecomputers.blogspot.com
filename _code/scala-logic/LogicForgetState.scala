trait LogicForgetState extends Logic {
  val LogicState: LogicState

  type T[A] = LogicState.T[Unit,A]

  def fail[A]: T[A] = LogicState.fail
  def unit[A](a: A): T[A] = LogicState.unit(a)
  def or[A](t1: T[A], t2: => T[A]): T[A] = LogicState.or(t1, t2)
  def bind[A,B](t: T[A], f: A => T[B]): T[B] = LogicState.bind(t, f)
  def apply[A,B](t: T[A], f: A => B): T[B] = LogicState.apply(t, f)
  def filter[A](t: T[A], p: A => Boolean): T[A] = LogicState.filter(t, p)
  def split[A](t: T[A]): Option[(A,T[A])] = {
    LogicState.split((), t) match {
      case None => None
      case Some((_, a, t)) => Some(a, t)
    }
  }
}
