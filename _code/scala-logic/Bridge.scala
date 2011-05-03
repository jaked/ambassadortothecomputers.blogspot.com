class Bridge[L <: Logic](val Logic: L) {
  import Logic._

  object Person extends Enumeration {
    type Person = Value
    val Alice, Bob, Candace, Dave = Value
    val all = List(Alice, Bob, Candace, Dave) // values is broken
  }
  import Person._

  val times = Map(Alice -> 5, Bob -> 10, Candace -> 20, Dave -> 25)

  case class State(left: List[Person],
                   lightOnLeft: Boolean,
                   timeRemaining: Int)

  def chooseTwo(list: List[Person]): T[(Person,Person)] =
    for { p1 <- or(list); p2 <- or(list); if p1 < p2 }
    yield (p1, p2)

  def next(state: State): T[State] = {
    if (state.lightOnLeft) {
      for {
        (p1, p2) <- chooseTwo(state.left)
        timeRemaining =
          state.timeRemaining - math.max(times(p1), times(p2))
        if timeRemaining >= 0
      } yield {
        val left =
          state.left.filterNot { p => p == p1 || p == p2 }
        State(left, false, timeRemaining)
      }
    } else {
      val right = Person.all.filterNot(state.left.contains)
      for {
        p <- or(right)
        timeRemaining = state.timeRemaining - times(p)
        if timeRemaining >= 0
      } yield State(p :: state.left, true, timeRemaining)
    }
  }

  def tree(path: List[State]): T[List[State]] =
    unit(path) |
      (for {
         state <- next(path.head)
         path <- tree(state :: path)
       } yield path)

  def search: T[List[State]] = {
    val start = List(State(Person.all, true, 60))
    for { path <- tree(start); if path.head.left == Nil }
    yield path
  }
}
