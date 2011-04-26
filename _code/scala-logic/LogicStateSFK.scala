object LogicStateSFK extends LogicState {
  type FK[R] = () => R
  type SK[S,A,R] = (S, A, FK[R]) => R

  trait T[S,A] { def apply[R](s: S, sk: SK[S,A,R], fk: FK[R]): R }

  def fail[S,A] =
    new T[S,A] {
      def apply[R](s: S, sk: SK[S,A,R], fk: FK[R]) = fk()
    }

  def unit[S,A](a: A) =
    new T[S,A] {
      def apply[R](s: S, sk: SK[S,A,R], fk: FK[R]) = sk(s, a, fk)
    }

  def or[S,A](t1: T[S,A], t2: => T[S,A]) =
    new T[S,A] {
      def apply[R](s: S, sk: SK[S,A,R], fk: FK[R]) = t1(s, sk, { () => t2(s, sk, fk) })
    }

  def bind[S,A,B](t: T[S,A], f: A => T[S,B]) =
    new T[S,B] {
      def apply[R](s: S, sk: SK[S,B,R], fk: FK[R]) =
        t(s, ({ (s, a, fk) => f(a)(s, sk, fk) }: SK[S,A,R]), fk)
    }

  def apply[S,A,B](t: T[S,A], f: A => B) =
    new T[S,B] {
      def apply[R](s: S, sk: SK[S,B,R], fk: FK[R]) =
        t(s, ({ (s, a, fk) => sk(s, f(a), fk) }: SK[S,A,R]), fk)
    }

  def filter[S,A](t: T[S,A], p: A => Boolean) =
    new T[S,A] {
      def apply[R](s: S, sk: SK[S,A,R], fk: FK[R]) =
        t(s, ({ (s, a, fk) => if (p(a)) sk(s, a, fk) else fk() }: SK[S,A,R]), fk)
    }

  def split[S,A](s: S, t: T[S,A]) = {
    def stateUnit[S,A](s: S, a: A): T[S,A] =
      new T[S,A] {
        def apply[R](s: S, sk: SK[S,A,R], fk: FK[R]) = sk(s, a, fk)
      }
    def unsplit(r: () => Option[(S,A,T[S,A])]): T[S,A] =
      r() match {
        case None => fail
        case Some((s, a, t)) => or(stateUnit(s, a), t)
      }
    def sk : SK[S,A,Option[(S,A,T[S,A])]] =
      { (s, a, fk) => Some((s, a, unit(fk).flatMap(unsplit))) }
    t(s, sk, { () => None })
  }

  def get[S]: T[S,S] =
    new T[S,S] {
      def apply[R](s: S, sk: SK[S,S,R], fk: FK[R]) = sk(s, s, fk)
    }

  def set[S](s: S): T[S,Unit] =
    new T[S,Unit] {
      def apply[R](_s: S, sk: SK[S,Unit,R], fk: FK[R]) = sk(s, (), fk)
    }
}
