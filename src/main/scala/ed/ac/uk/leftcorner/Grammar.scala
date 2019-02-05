package ed.ac.uk.leftcorner

import ed.ac.uk.kanren.{KList, SearchStrategy, SearchStream}

import scala.io.Source

class Grammar(lexicon:List[(String, List[String])]) {

  private val licensors = lexicon.flatMap(_._2).filter(_ startsWith "+").distinct.map(_.tail)

  val (empties, nonEmpties) = lexicon.partition(_._1.matches("|ε|\\[.*\\]"))

  val mgLogicEager = new MGlogic(licensors, eagerMove=true)
  val mgLogicStandard = new MGlogic(licensors, eagerMove=false)

  def parse(sent:List[String], eagerMove:Boolean=true, maxAllowedTransitions : Int= -1)(implicit searchStrategy: SearchStrategy) : Stream[Node] = {
    val initParseState = ParseState.initState(
      words = sent,
      maxAllowedTransitions = if(maxAllowedTransitions<0) 3*sent.size else maxAllowedTransitions,
      mgLogic = if(eagerMove) mgLogicEager else mgLogicStandard,
      grammar = this
    )
    val parseStates = fullExpansion(initParseState)
    parseStates.toScalaStream.map(_.tree)
  }

  private def fullExpansion(state : ParseState)(implicit searchStrategy: SearchStrategy) : SearchStream[ParseState] =
    SearchStream.fromSeq(state.singleExpansion).flatMap(subState =>
      if(subState.isFinal)
        SearchStream(subState)
      else
        fullExpansion(subState)
    )

}

object Grammar{

  def loadFromFile(fn:String) : Grammar = {
    val lexicon = Source.fromFile(fn).getLines().filterNot(_.matches("^ *$")).filterNot(_.matches("^ *%.*")).map{ line =>
      val parts = line split " *:: *"
      val word = parts(0).replaceAll(" ", "")
      val feats = parts(1).split(" +").toList.filterNot(_.matches(" *"))
      (word, feats)
    }.toList
    new Grammar(lexicon)
  }

}
