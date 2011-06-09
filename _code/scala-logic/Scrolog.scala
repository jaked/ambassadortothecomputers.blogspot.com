trait Scrolog {
  val LogicState: LogicState
  import LogicState._

  type G = T[Env,Unit]

  class TermSyntax[A](t: Term[A]) {
    def =:=(t2: Term[A]): G =
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

  def printEnv: G = for (env <- get) yield Console.println(env)
  def println(m: String): G = unit(Console.println(m))

  implicit def termSyntax[A](t: Term[A]) = new TermSyntax(t)
  implicit def syntax[A](t: G) = LogicState.syntax(t)

  def run[A](t: G, n: Int, tm: Term[A]): List[Term[A]] =
    LogicState.run(Env.empty, t, n)
      .map({ case (e, _) => tm.subst(e) })
}
