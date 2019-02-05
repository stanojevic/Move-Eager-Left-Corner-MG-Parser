package ed.ac.uk.kanren

sealed trait Term{

  def asStr   : String     = this.asInstanceOf[Val].content.asInstanceOf[String]
  def asInt   : Int        = this.asInstanceOf[Val].content.asInstanceOf[Int]
  def asSymb  : Symbol     = this.asInstanceOf[Val].content.asInstanceOf[Symbol]
  def asKList : KList      = this.asInstanceOf[KList]
  def asList  : List[Term] = this.asInstanceOf[KList].toScalaStream.toList

  private val x = this

  private[kanren] def occurs(y:Var) : Boolean = x match {
    case _:Var                => y==x
    case KCons(l, r)          => l.occurs(y) || r.occurs(y)
    case ImproperEndOfList(_) => throw new Exception("there should be no object of this type here")
    case _                    => false
  }

  private def unify(y:Term)(s:Substitution) : Option[Substitution] = (s.walk(x), s.walk(y)) match {
    case (x            , y            ) if x == y => Some(s)
    case (x:Var        , y            )           => s.extend(Association(x, y))
    case (x            , y:Var        )           => s.extend(Association(y, x))
    case (KCons(xl, xr), KCons(yl, yr))           => Some(s).flatMap(xl unify yl).flatMap(xr unify yr)
    case _                                        => None
  }

  def ===(y:Term) : Goal = (s: Substitution) => (x unify y) (s) match {
    case None => SearchStream.empty
    case Some(sub) => SearchStream(sub)
  }

  def toReifiedVarsNormalForm : Term = this match {
    case KCons(h, t) => KCons(h.toReifiedVarsNormalForm, t.toReifiedVarsNormalForm)
    case v:Var => new Var(v.name.toString.tail.toLong, v.name)
    case x => x
  }

}

object Var{

  private var counter = 0l
  private def nextId:Long = {
    counter += 1
    counter
  }

  def newReified(n:Int) : Var =
    newVar(Symbol("_"+n))

  private def newVarUnnamed() : Var =
    newVar(Symbol("__V"+nextId))

  private def newVar(name:Symbol) : Var =
    new Var(nextId, name)

  def apply() : Var =
    newVarUnnamed()

  def apply(name:Symbol) : Var =
    newVar(name)

}

class Var private[kanren] (val id:Long, val name:Symbol) extends Term {

  override def hashCode(): Int = id.##

  override def equals(obj: scala.Any): Boolean = obj match {
    case v:Var => v.id == id
    case _ => false
  }

  override def toString: String = Option(name) match {
    case Some(symbol) => symbol.toString()
    case None => s"var_$id"
  }

}

case class Val(content:Any) extends Term {

  override def toString: String = content.toString

}

/**
  * the reason for using special KList instead of Scala's standard lists is
  * - for having "improper lists" that are possible in Scheme but not in standard Scala lists
  * - for having all terms (including lists) implement Term interface
  */
sealed trait KList extends Term {

  def toScalaStream : Stream[Term] = this match {
    case KCons(head, tail:KList) => head #:: tail.toScalaStream
    case KCons(head, tail:Term) => head #:: ImproperEndOfList(tail) #:: Stream.empty[Term]
    case KNil => Stream.empty
  }

  override final def toString: String = "["+toScalaStream.mkString(", ")+"]"

  def apply(i:Int) : Term = this match {
    case KCons(head, _         ) if i == 0 => head
    case KCons(_   , tail:KList)           => tail(i-1)
    case _ => sys.error(s"can access element $i in KList $this")
  }

}

// used only for expressing improper lists in Scala
case class ImproperEndOfList(lastElem:Term) extends Term {
  override def toString: String = s". $lastElem"
}

object KList{

  def fromList(xs:List[Term]) : KList =
    xs.foldRight(empty){ (x, l) => KCons(x, l)}

  def fromListRecursive(xs:AnyRef) : Term = xs match {
    case xs: List[_] => fromList(xs.map(x => fromListRecursive(x.asInstanceOf[AnyRef])))
    case (x1:AnyRef, x2:AnyRef) => KList(fromListRecursive(x1), fromListRecursive(x2))
    case (x1:AnyRef, x2:AnyRef, x3:AnyRef) => KList(fromListRecursive(x1), fromListRecursive(x2), fromListRecursive(x3))
    case (x1:AnyRef, x2:AnyRef, x3:AnyRef, x4:AnyRef) => KList(fromListRecursive(x1), fromListRecursive(x2), fromListRecursive(x3), fromListRecursive(x4))
    case t : Term    => t
    case x : AnyRef  => Val(x)
  }

  def apply(xs:Term*) : KList =
    xs.foldRight(empty){ (x, l) => KCons(x, l)}

  def empty : KList = KNil

  def unapplySeq(arg: KList): Option[Seq[Term]] = Some(arg.toScalaStream)

}

case object KNil extends KList

case class KCons(head:Term, tail:Term) extends KList


