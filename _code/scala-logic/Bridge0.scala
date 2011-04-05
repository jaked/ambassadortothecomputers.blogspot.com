object Bridge0 {
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

  def chooseTwo(list: List[Person]): List[(Person,Person)] = {
    val init: List[(Person, Person)] = Nil
    list.foldLeft(init) { (pairs, p1) =>
      list.foldLeft(pairs) { (pairs, p2) =>
        if (p1 < p2) (p1, p2) :: pairs else pairs
      }
    }
  }

  def next(state: State): List[State] = {
    if (state.lightOnLeft) {
      val init: List[State] = Nil
      chooseTwo(state.left).foldLeft(init) {
        case (states, (p1, p2)) =>
          val timeRemaining =
            state.timeRemaining - math.max(times(p1), times(p2))
          if (timeRemaining >= 0) {
            val left =
              state.left.filterNot { p => p == p1 || p == p2 }
            State(left, false, timeRemaining) :: states
          }
          else
            states
      }
    } else {
      val right = Person.all.filterNot(state.left.contains)
      val init: List[State] = Nil
      right.foldLeft(init) { (states, p) =>
        val timeRemaining = state.timeRemaining - times(p)
        if (timeRemaining >= 0)
          State(p :: state.left, true, timeRemaining) :: states
        else
          states
      }
    }
  }

  def tree(path: List[State]): List[List[State]] =
    next(path.head).
      map(s => tree(s :: path)).
        foldLeft(List(path)) { _ ++ _ }

  def search: List[List[State]] = {
    val start = List(State(Person.all, true, 60))
    tree(start).filter { _.head.left == Nil }
  }
}
