object Test {
  import Term._
  import LogicStateSKE._

  def member[A](x: Term[A], l: Term[List[A]]): T[Env,Unit] = {
    val hd = Evar[A]("hd"); val tl = Evar[List[A]]("tl")
    ConsTerm(hd, tl) =!= l &
    (x =!= hd | member(x, tl))
  }
}
