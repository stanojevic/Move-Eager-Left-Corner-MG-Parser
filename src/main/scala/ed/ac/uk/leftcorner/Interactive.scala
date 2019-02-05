package ed.ac.uk.leftcorner

object Interactive {

  private var mgLogic : MGlogic = _
  private var grammar : Grammar = _
  var state : ParseState = _

  def loadGrammar(fn:String) : Unit = {
    grammar = Grammar.loadFromFile(fn)
    println("grammar loaded")
    useEagerMove
  }

  def useEagerMove : Unit = {
    if(grammar == null) System.err.println("load grammar first") else mgLogic = grammar.mgLogicEager
    println("using eager move")
  }

  def useStandardMove : Unit = {
    if(grammar == null) System.err.println("load grammar first") else mgLogic = grammar.mgLogicStandard
    println("using standard move")
  }

  def resetParseState(words:List[String]) : Unit = {
    state = ParseState.initState(words = words, maxAllowedTransitions = 100000, grammar=grammar, mgLogic=mgLogic)
    println("parser state resetted")
  }

  def options : Unit = state.singleExpansion.zipWithIndex.foreach{case (a, i) => println(s"option $i : \n\t$a")}

  def takeAction(i:Int) : Unit = {
    state = state.singleExpansion(i)
    println(state)
    if(state.isFinal){
      println("Parsing finished successfully!")
      println(state.tree)
      println("you can visualize result with state.tree.visualize()")
    }
  }

}
