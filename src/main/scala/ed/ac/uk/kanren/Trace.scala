package ed.ac.uk.kanren

import scala.collection.mutable.{Map => MutMap}

object Trace{

  def apply(name:String)(g: => Goal) : Trace =
    new Trace(name)(g)

  private val nextIntMap = MutMap[String, Int]()
  private def nextInt(name:String): Int = {
    nextIntMap(name) = nextIntMap.getOrElse(name, -1) + 1
    nextIntMap(name)
  }

}

class Trace(name:String)(g: => Goal) extends Goal{
  private val n = Trace.nextInt(name)
  if(Kanren.TRACE)
    System.err.println(s"BUILD $name$n")
  override def apply(s: Substitution): SearchStream[Substitution] = {
    if(Kanren.TRACE)
      System.err.println(s"ENTER $name$n\t -> $s")
    val res = g(s)
    if(Kanren.TRACE)
      System.err.println(s"LEAVE $name$n\t -> $res")
    res
  }
}

