package ed.ac.uk.kanren

case class Association(x:Var, y:Term){
  def matches(x:Var) : Boolean = x==this.x

  override def toString: String = s"$x = $y"
}

private[kanren] class Substitution(val subList:List[Association]) extends Iterable[Association]{

  override def toString(): String =
    "Substitutions ==> "+subList.map(_.toString).sorted.mkString(", ")

  def walkStar(x:Term) : Term = walk(x) match {
    case KCons(l, r) =>
      KCons(walkStar(l), walkStar(r))
    case y =>
      y
  }

  def walk(x:Term) : Term = x match {
    case x:Var   =>
      subList.find(_.matches(x)) match {
        case None => x
        case Some(Association(_, y:Var)) => walk(y)
        case Some(Association(_, y    )) => y
        // case _ => throw new RuntimeException
      }
    case _ =>
      x
  }

  def extend(a:Association) : Option[Substitution] =
    if(a.y.occurs(a.x))
      None
    else
      Some( new Substitution(a :: subList) )

  override def iterator: Iterator[Association] = subList.iterator

  private def reifyS(v:Term) : Substitution =
    walk(v) match {
      case x:Var if v == x =>
        new Substitution(Association(x, Var.newReified(subList.size)) :: subList)
      case KCons(l, r) =>
        reifyS(l).reifyS(r)
      case _ =>
        this
    }

  def reify(v:Term) : Term = {
    val vv = walkStar(v)
    val r = Substitution.empty.reifyS(vv)
    r.walkStar(vv)
  }

}


object Substitution{
  def empty : Substitution = new Substitution(List())
}


