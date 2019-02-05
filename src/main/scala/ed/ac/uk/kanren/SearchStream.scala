package ed.ac.uk.kanren

/**
  * The reasons for using special streams instead of standard scala streams are:
  * - Scala streams always evaluate their first argument which can sometimes trigger infinite recursion
  * - Scala streams ++ would cause DFS while with special streams we can have both DFS and BFS (default)
  */

trait SearchStrategy
case object BFS extends SearchStrategy
case object DFS extends SearchStrategy
object SearchStrategy{
  val BFS = ed.ac.uk.kanren.BFS
  val DFS = ed.ac.uk.kanren.DFS
}

object SearchStream{

  def apply[T](subs:T*) : SearchStream[T] =
    subs.foldRight[SearchStream[T]](SearchStream.empty){ (sub, stream) => SearchStreamCons(sub, stream)}

  def empty[T] : SearchStream[T] = new SearchStreamEmpty[T]

  def fromSeq[T](st: => Seq[T]) : SearchStream[T] = SearchStreamSuspension(
    st match {
      case Stream.Empty => empty
      case h #:: tail => SearchStreamCons(h, fromSeq(tail))
      case Nil => empty
      case h :: tail => SearchStreamCons(h, fromSeq(tail))
      case _ => throw new Exception("unsupported type of sequence")
    }
  )

}

sealed trait SearchStream[T] {

  def partiallyForceEval : SearchStream[T] = this match {
    case stream : SearchStreamSuspension[T] => stream.content.partiallyForceEval
    case _ => this
  }

  def toScalaStream : Stream[T] = this match {
    case _ :SearchStreamEmpty[T]       => Stream.empty[T]
    case st:SearchStreamCons[T]        => st.head #:: st.tail.toScalaStream
    case st:SearchStreamSuspension[T]  => st.content.toScalaStream
  }

  override def toString: String = "StreamKanren("+toStringRec(this)+")"
  private def toStringRec : SearchStream[T] => String = {
    case _ :SearchStreamEmpty[T]      => ""
    case _ :SearchStreamSuspension[T] => "--Suspension--"
    case st:SearchStreamCons[T]       => st.head.toString + "\t;\t" + toStringRec(st.tail)
  }

  def ++ (y: => SearchStream[T])(implicit searchStrategy: SearchStrategy) : SearchStream[T] = this match {
    case _:SearchStreamEmpty[T] =>
      // y
      SearchStreamSuspension(y)
    case st:SearchStreamCons[T] =>
      SearchStreamCons(st.head, SearchStreamSuspension( st.tail ++ y ))
    case st:SearchStreamSuspension[T] =>
      SearchStreamSuspension(
        searchStrategy match{
          case BFS =>
            y ++ st.content // breadth first
          case DFS =>
            st.content ++ y // depth first
        }
      )
    case _ =>
      throw new RuntimeException
  }

  def flatMapSeq(goal: T => Seq[T])(implicit searchStrategy: SearchStrategy) : SearchStream[T] = flatMap( { s:T => SearchStream.fromSeq(goal(s))} )

  def flatMap(goal: T => SearchStream[T])(implicit searchStrategy: SearchStrategy) : SearchStream[T] = this match {
    case e:SearchStreamEmpty[T] =>
      e
    case st:SearchStreamCons[T] =>
      goal(st.head) ++ st.tail.flatMap(goal)
    case st:SearchStreamSuspension[T] =>
      SearchStreamSuspension( st.content.flatMap(goal) )
  }

  def take(k:Int) : Stream[T] = (k, this) match {
    case (0, _                              ) => Stream.empty
    case (_, _:SearchStreamEmpty[T]         ) => Stream.empty
    case (_, st:SearchStreamCons[T]         ) => st.head #:: st.tail.take(k-1)
    case (_, st:SearchStreamSuspension[T]   ) => st.content.take(k)
    case _                                    => throw new RuntimeException
  }

}

/////////////////////////       Empty        ///////////////////////////

private[kanren] class SearchStreamEmpty[T]() extends SearchStream[T]

/////////////////////////       Cons        ///////////////////////////

private[kanren] class SearchStreamCons[T] private(l: => T, r: => SearchStream[T]) extends SearchStream[T]{
  lazy val head = l
  lazy val tail = r
}

private[kanren] object SearchStreamCons{
  def apply[T](l: => T, r: => SearchStream[T]) : SearchStream[T] = new SearchStreamCons(l, r)
}

/////////////////////////       Suspension        ///////////////////////////

private[kanren] class SearchStreamSuspension[T] private(lambda: => SearchStream[T]) extends SearchStream[T] {
  lazy val content:SearchStream[T] = lambda
}

private[kanren] object SearchStreamSuspension{
  def apply[T](lambda: => SearchStream[T]) : SearchStreamSuspension[T] = new SearchStreamSuspension[T](lambda)
}
