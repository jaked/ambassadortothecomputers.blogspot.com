trait LogicState { L =>
  type T[S,A]

  def fail[S,A]: T[S,A]
  def unit[S,A](a: A): T[S,A]
  def or[S,A](t1: T[S,A], t2: => T[S,A]): T[S,A]
  def apply[S,A,B](t: T[S,A], f: A => B): T[S,B]
  def bind[S,A,B](t: T[S,A], f: A => T[S,B]): T[S,B]
  def filter[S,A](t: T[S,A], p: A => Boolean): T[S,A]
  def split[S,A](s: S, t: T[S,A]): Option[(S,A,T[S,A])]

  def get[S]: T[S,S]
  def set[S](s: S): T[S, Unit]

  def or[S,A](as: List[A]): T[S,A] =
    as.foldRight(fail[S,A])((a, t) => or(unit(a), t))

  def run[S,A](s0: S, t: T[S,A], n: Int): List[(S,A)] = {
    def runAcc(t: T[S,A], n: Int, acc: List[(S,A)]): List[(S,A)] =
      if (n <= 0) acc.reverse else
        split(s0, t) match {
          case None => acc
          case Some((s, a, t)) => runAcc(t, n - 1, (s, a) :: acc)
        }
    runAcc(t, n, Nil)
  }

  case class Syntax[S,A](t: T[S,A]) {
    def map[B](f: A => B): T[S,B] = L.apply(t, f)
    def filter(p: A => Boolean): T[S,A] = L.filter(t, p)
    def flatMap[B](f: A => T[S,B]): T[S,B] = L.bind(t, f)
    def withFilter(p: A => Boolean): T[S,A] = L.filter(t, p)

    def |(t2: => T[S,A]): T[S,A] = L.or(t, t2)
    def &[B](t2: => T[S,B]): T[S,B] = L.bind(t, { _: A => t2 })
  }

  implicit def syntax[S,A](t: T[S,A]) = Syntax(t)
}
