trait Logic { L =>
  type T[A]

  def fail[A]: T[A]
  def unit[A](a: A): T[A]
  def or[A](t1: T[A], t2: => T[A]): T[A]
  def apply[A,B](t: T[A], f: A => B): T[B]
  def bind[A,B](t: T[A], f: A => T[B]): T[B]
  def filter[A](t: T[A], p: A => Boolean): T[A]
  def split[A](t: T[A]): Option[(A,T[A])]

  def or[A](as: List[A]): T[A] =
    as.foldRight(fail[A])((a, t) => or(unit(a), t))

  def run[A](t: T[A], n: Int): List[A] = {
    def runAcc(t: T[A], n: Int, acc: List[A]): List[A] =
      if (n <= 0) acc.reverse else
        split(t) match {
          case None => acc
          case Some((a, t)) => runAcc(t, n - 1, a :: acc)
        }
    runAcc(t, n, Nil)
  }

  case class Syntax[A](t: T[A]) {
    def map[B](f: A => B): T[B] = L.apply(t, f)
    def filter(p: A => Boolean): T[A] = L.filter(t, p)
    def flatMap[B](f: A => T[B]): T[B] = L.bind(t, f)
    def withFilter(p: A => Boolean): T[A] = L.filter(t, p)

    def |(t2: => T[A]): T[A] = L.or(t, t2)
  }

  implicit def syntax[A](t: T[A]) = Syntax(t)
}
