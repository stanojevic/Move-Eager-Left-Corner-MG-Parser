package ed.ac.uk.kanren

trait Goal{

  def apply(s: Substitution): SearchStream[Substitution]

  private val left = this // just because it's easier to refer to "this" as "left"

  // // aliases for || and &&
  // def |  (right: => Goal) : Goal = this || right
  // def &  (right: => Goal) : Goal = this && right
  // def or (right: => Goal) : Goal = this || right
  // def and(right: => Goal) : Goal = this && right

  def ||(right: => Goal) : Goal = {
    implicit val strategy = Kanren.searchStrategy
    s => left(s) ++ right(s)
  }

  def &&(right: => Goal) : Goal = {
    implicit val strategy = Kanren.searchStrategy
    s => SearchStream(s).flatMap(left(_)).flatMap(right(_))
  }

}

