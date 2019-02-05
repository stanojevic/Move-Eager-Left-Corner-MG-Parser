package ed.ac.uk.leftcorner

import ed.ac.uk.kanren.Kanren._
import ed.ac.uk.kanren.{KList, Val}

object ParseState {

  def initState(words:List[String], maxAllowedTransitions : Int, mgLogic: MGlogic, grammar: Grammar) : ParseState =
    ParseState(
      transitionsSoFar = 0,
      maxAllowedTransitions = maxAllowedTransitions,
      position = 0,
      stack = KList(mgLogic.rootSlash),
      buffer = words,
      prevState = None,
      transition = None,
      mgLogic = mgLogic,
      grammar = grammar
    )

}

sealed case class ParseState(
                              transitionsSoFar      : Int,
                              maxAllowedTransitions : Int,
                              position              : Int,
                              stack                 : Term,
                              buffer                : List[String],
                              prevState             : Option[ParseState],
                              transition            : Option[String],
                              mgLogic               : MGlogic,
                              grammar               : Grammar
                            ){

  def singleExpansion : Seq[ParseState] =
    if(isFinal) {
      List(this)
    }else if(transitionsSoFar>= maxAllowedTransitions){
      Nil
    }else{
      var options = List[ParseState]()

      options ++= computeOptions(nonEmptyShift = false, mgLogic.transition_predict_O)
      options ++= computeOptions(nonEmptyShift = false, mgLogic.transition_predictConnect_O)

      if(buffer.nonEmpty){
        for((word, feats) <- grammar.nonEmpties if word == buffer.head){
          val newItem = mgLogic.init_MG_item(position, position+1, word, feats)
          options ++= computeOptions(nonEmptyShift = true, mgLogic.transition_shiftConnect_O(_, newItem, _, _))
          options ++= computeOptions(nonEmptyShift = true, mgLogic.transition_shift_O       (_, newItem, _, _))
        }
      }

      for((word, feats) <- grammar.empties){
        val newItem = mgLogic.init_MG_item(position, position, word, feats)
        options ++= computeOptions(nonEmptyShift = false, mgLogic.transition_shiftConnect_O(_, newItem, _, _))
        options ++= computeOptions(nonEmptyShift = false, mgLogic.transition_shift_O       (_, newItem, _, _))
      }

      options
    }

  private def computeOptions(nonEmptyShift:Boolean, goal: (Term, Term, Term) => Goal) : Seq[ParseState] =
    run( (stackOut, transName) =>
      goal(stack, stackOut, transName)
    ).map{ case (newStack, transName) =>
      this.copy(
        stack = newStack,
        transition = Some(flattenName(transName)),
        prevState = Some(this),
        position = if(nonEmptyShift) position+1 else position,
        buffer = if(nonEmptyShift) buffer.tail else buffer,
        transitionsSoFar = transitionsSoFar+1
      )
    }

  private def flattenName(name:Term) : String = name match {
    case x:KList => x.toScalaStream.map(flattenName).mkString("-")
    case Val(x) => x.toString
    case ImproperEndOfList(lastElem) => lastElem.toString
    case _ => sys.error("wtf")
  }

  lazy val isFinal : Boolean = stack match {
    case KList(KList(Val("ROOT"), _*)) => true
    case _ => false
  }

  override def toString: String =
    transition match {
      case None =>
        "init state\n"+
        "\tstack[0] : "+MGlogic.itemToString(stack.asKList.toScalaStream.head)
      case Some(t) =>
        s"transition $t\n"+
        stack.asKList.toScalaStream.zipWithIndex.map{ case (e, i) =>
          s"\tstack[$i] : "+MGlogic.itemToString(e)
        }.mkString("\n")
    }

  def printTransitionTrace() : Unit =
    statesInOrder.foreach(println)

  private def statesInOrder : List[ParseState] = prevState match {
    case Some(prev) => prev.statesInOrder :+ this
    case None       => this :: Nil
  }

  def tree : Node = {
    assert(isFinal)
    Node.recTransform(stack.asKList(0).asKList(2))
  }

}

