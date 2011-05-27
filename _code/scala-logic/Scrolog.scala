trait Scrolog {
  val LogicState: LogicState
  import LogicState._

  type P = T[Env,Unit]

  class TermSyntax[A](t: Term[A]) {
    def =:=(t2: Term[A]): P =
      for {
        env <- get
        env2 <- {
          t.subst(env).unify(env, t2.subst(env)) match {
            case None => fail[Env,Unit]
            case Some(e) => set(e)
          }
        }
      } yield env2
  }

  def printEnv: P = for (env <- get) yield Console.println(env)
  def println(m: String): P = unit(Console.println(m))

  implicit def termSyntax[A](t: Term[A]) = new TermSyntax(t)
  implicit def varTermSyntax[A](t: VarTerm[A]) = new TermSyntax(t)
  implicit def litTermSyntax[A](t: LitTerm[A]) = new TermSyntax(t)
  implicit def tuple2TermSyntax[A,B](t: Tuple2Term[A,B]) = new TermSyntax(t)
  implicit def nilTermSyntax[A](t: NilTerm[A]) = new TermSyntax(t)
  implicit def consTermSyntax[A](t: ConsTerm[A]) = new TermSyntax(t)

  implicit def syntax[A](t: P) = LogicState.syntax(t)

  def run[A](t: P, n: Int, tm: Term[A]): List[Term[A]] =
    LogicState.run(Env.empty, t, n).map({ case (e, _) => tm.subst(e) })
}
