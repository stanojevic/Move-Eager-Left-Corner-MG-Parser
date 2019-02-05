package ed.ac.uk.leftcorner

object Main {

  def main(args:Array[String]) : Unit =
    exampleEagerMove()

  def exampleEagerMove() : Unit = {
    val g = Grammar.loadFromFile("./src/main/scala/ed/ac/uk/leftcorner/grammars/G1.txt")

    val initState = ParseState.initState(
      words = "Bowie likes what Iggy sings".split(" ").toList,
      maxAllowedTransitions = 20,
      mgLogic = g.mgLogicEager,
      grammar = g
    )

    var state = initState
    state = state.singleExpansion(1)
    state = state.singleExpansion(2)
    state = state.singleExpansion(0)
    state = state.singleExpansion(13)
    state = state.singleExpansion(0)
    state = state.singleExpansion(2)
    state = state.singleExpansion(0)
    state = state.singleExpansion(1)
    state = state.singleExpansion(2)
    state = state.singleExpansion(4)
    state = state.singleExpansion(0)
    state = state.singleExpansion(15)
    state = state.singleExpansion(0)

    println()
    state.printTransitionTrace()

    println()
    println(state.tree)

    state.tree.visualize("resulting tree")

  }

  def exampleNonEagerMove() : Unit = {
    val g = Grammar.loadFromFile("./src/main/scala/ed/ac/uk/leftcorner/grammars/G1.txt")

    val initState = ParseState.initState(
      words = "Bowie likes what Iggy sings".split(" ").toList,
      maxAllowedTransitions = 20,
      mgLogic = g.mgLogicStandard,
      grammar = g
    )

    var state = initState
    state = state.singleExpansion(1)
    state = state.singleExpansion(2)
    state = state.singleExpansion(0)
    state = state.singleExpansion(3)
    state = state.singleExpansion(0)
    state = state.singleExpansion(2)
    state = state.singleExpansion(0)
    state = state.singleExpansion(0)
    state = state.singleExpansion(2)
    state = state.singleExpansion(0) // key step -- has no eager move
    state = state.singleExpansion(0)
    state = state.singleExpansion(5)
    state = state.singleExpansion(0)
    state = state.singleExpansion(1) // move and connect

    println()
    state.printTransitionTrace()

    println()
    println(state.tree)

    state.tree.visualize("resulting tree")

  }

  def exampleParse() : Unit = {

    implicit val searchStrategy = ed.ac.uk.kanren.DFS

    val g = Grammar.loadFromFile("/home/milos/Projects/CCG-translator/src/main/scala/edin/leftcorner/grammars/G1.txt")

    val trees:Stream[Node] = g.parse(sent = "Aca knows what Bibi likes".split(" ").toList, maxAllowedTransitions = 15)

    println("parsing started")
    for(tree <- trees){
      println("parse tree")
      println(tree)
    }
    println("parsing ended")

  }

}

