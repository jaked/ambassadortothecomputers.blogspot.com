object LogicList extends Logic {
  type T[A] = List[A]

  def fail[A] = Nil
  def unit[A](a: A) = a :: Nil
  def or[A](t1: List[A], t2: => List[A]) = t1 ::: t2
  def apply[A,B](t: List[A], f: A => B) = t.map(f)
  def bind[A,B](t: List[A], f: A => List[B]) = t.flatMap(f)
  def filter[A](t: List[A], p: A => Boolean) = t.filter(p)
  def split[A](t: List[A]) =
    t match {
      case Nil => None
      case h :: t => Some(h, t)
    }
}
