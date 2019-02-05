package ed.ac.uk.leftcorner

import ed.ac.uk.kanren.Kanren._
import ed.ac.uk.kanren.KanrenList._
import ed.ac.uk.kanren.KanrenImpure.project
import ed.ac.uk.kanren.Substitution

/**
  * This class contains all the logical relations for MG LC parsing
  * as a prerequisite the only thing needed is to prespecify all the licensor
  * features (wh, case etc.) so that it could control for SMC even when
  * movers are unknown
  */
object MGlogic{

  case object NO_MOVER
  case object Selector
  case object Selectee
  case object Licensor
  case object Licensee

  def itemToString(item:Term) : String = item match {
    case KList(Val("ROOT"), end, _*) => s"(0, $end) ROOT"
    case KList(Val("slashed"), antecedent, Val("=>"), consequent) => itemToString(antecedent) + "\t=>\t" + itemToString(consequent)
    case KList(start, end, typee, feats, movers, _, _) => s"{($start, $end)    $typee    "+featsToString(feats)+", "+moversToString(movers)+"}"
  }

  private def featsToString(feats:Term) : String = feats match {
    case fs:KList =>
      fs.toScalaStream.flatMap{
        case KList(Val(Selectee), f) =>
          s"$f" :: Nil
        case KList(Val(Selector), f) =>
          s"=$f" :: Nil
        case KList(Val(Licensee), f) =>
          s"-$f" :: Nil
        case KList(Val(Licensor), f) =>
          s"+$f" :: Nil
        case ImproperEndOfList(lastElem) =>
          "." :: lastElem :: Nil
        case x                       =>
          x.toString :: Nil
      }.mkString(" ")
    case _ =>
      feats.toString
  }

  private def moversToString(movers:Term) : String = movers.asInstanceOf[KList].toScalaStream.map{
    case KList(Val(f), Val(NO_MOVER))            => s"[$f: NO_MOVER]"
    case KList(Val(f), ImproperEndOfList(v:Var)) => s"[$f: $v...]"
    case KList(Val(f), start, end, feats)        => s"[$f: ($start, $end)"+featsToString(feats)+"]"
  }.mkString(", ")

}

class MGlogic(licensors:List[String], eagerMove:Boolean) {

  import MGlogic._

  private val selectorRx = "=([^=+-]+)".r
  private val selecteeRx = "([^=+-]+)".r
  private val licensorRx = "\\+([^=+-]+)".r
  private val licenseeRx = "-([^=+-]+)".r

  private def moversListEmpty : KList = KList.fromList(licensors.map(list(_, NO_MOVER)))

  def rootSlash : Term = {
    val tree = Var()
    val end = Var()
    Substitution.empty.reify(
      list("slashed",
        list(0, end, __, list(list(Selectee, "c")), moversListEmpty, tree, list()),
        "=>",
        KList("ROOT", end, tree)
      )
    )
  }

  private def transformFeaturesF(features:List[String]) : List[Term] =
    features.map(feat)

  private def feat : String => Term = {
    case selectorRx(f) => list(Selector, f)
    case selecteeRx(f) => list(Selectee, f)
    case licensorRx(f) => list(Licensor, f)
    case licenseeRx(f) => list(Licensee, f)
  }

  private def constructTreeForUnification(start:Term, end:Term, word:String, features:List[String]) : (Term, Term) = {
    val bottomNode = list(start, end, word, features.mkString(" "))
    val argFeats:List[String] = features.takeWhile(x => x.startsWith("+") || x.startsWith("="))
    val (vars, tree) = argFeats.zipWithIndex.foldLeft[(List[(Var, Var)], Term)]((List[(Var, Var)](), bottomNode)){ (res, featIndexed) =>
      val (feat, i) = featIndexed
      if(feat.startsWith("=")){
        val subTree = Var()
        val mergeType = Var()
        val vars = res._1 :+ (subTree, mergeType)
        val tree = if(i==0) list(mergeType, res._2, subTree) else list(mergeType, subTree, res._2)
        (vars, tree)
      }else{
        val subTree = __
        val moveType = Var()
        val vars = res._1 :+ (subTree, moveType)
        val tree = list(moveType, res._2)
        (vars, tree)
      }
    }
    (KList.fromListRecursive(vars), tree)
  }

  def init_MG_item_empty(features:List[String]) : Term = {
    val position = Var()
    val (vars, tree) = constructTreeForUnification(position, position, "", features)
    list(
      position,
      position,
      "::",
      list.fromList(transformFeaturesF(features)),
      list.fromList(licensors.map(list(_, NO_MOVER))),
      tree,
      vars
    )
  }

  def init_MG_item(start:Int, end:Int, word:String, features:List[String]) : Term = {
    val (vars, tree) = constructTreeForUnification(start, end, word, features)
    list(
      start,
      end,
      "::",
      list.fromList(transformFeaturesF(features)),
      list.fromList(licensors.map(list(_, NO_MOVER))),
      tree,
      vars
    )
  }

  private def moversListO(movers:Term) : Goal = KList.fromList(licensors.map(cons(_, __))) === movers

  def mgItemO(item:Term) : Goal =
    mgItemO(item, __, __, __, __, __, __, __)

  def mgItemO(item:Term, start:Term, end:Term, typee:Term, features:Term, movers:Term, tree:Term, argVars:Term) : Goal =
    item === list(start, end, typee, features, movers, tree, argVars) && moversListO(movers)

  // A => B
  def slashItemO(antecedent:Term, consequent:Term, slashed:Term) : Goal = (
    mgItemO(antecedent),
    // mgItemO(consequent),
    slashed === list("slashed", antecedent, "=>", consequent)
  )

  def composeTernaryDirectedO(slashed1:Term, slashed2:Term, slashed3:Term, result:Term) : Goal = fresh((a, b, c, d) => (
    slashItemO(a, b, slashed1),
    slashItemO(b, c, slashed2),
    slashItemO(c, d, slashed3),
    slashItemO(a, d, result)
  ))

  def composeBinaryDirectedO(slashed1:Term, slashed2:Term, result:Term) : Goal = fresh((a, b, c) => (
    slashItemO(a, b, slashed1),
    slashItemO(b, c, slashed2),
    slashItemO(a, c, result)
  ))

  private def c0_O(stackIn:Term, stackOut:Term) : Goal = fresh( (focus, focusNew, rest0, rest1, candidate) => (
    consO(focus, rest0, stackIn),
//    mgItemO(focus),
    selectO(candidate, rest0, rest1),
    //    slashItemO(__, __, candidate),
//    applyBinaryUndirected(candidate, focus, focusNew, __),
    slashItemO(focus, focusNew, candidate),
    consO(focusNew, rest1, stackOut)
  ))

  private def c1_O(stackIn:Term, stackOut:Term) : Goal = fresh( (focus, focusNew, rest0, rest1, candidate) => (
    consO(focus, rest0, stackIn),
//    slashItemO(__, __, focus),
    selectO(candidate, rest0, rest1),
//    slashItemO(__, __, candidate),
    composeBinaryDirectedO(candidate, focus, focusNew),
    consO(focusNew, rest1, stackOut)
  ))

  private def c2_O(stackIn:Term, stackOut:Term) : Goal = fresh( (focus, focusNew, rest0, rest1, candidate) => (
    consO(focus, rest0, stackIn),
//    slashItemO(__, __, focus),
    selectO(candidate, rest0, rest1),
    //    slashItemO(__, __, candidate),
    composeBinaryDirectedO(focus, candidate, focusNew),
    consO(focusNew, rest1, stackOut)
  ))

  def c3_O(stackIn:Term, stackOut:Term) : Goal = fresh( (focus, focusNew, rest0, rest1, rest2, candidateA, candidateB) => (
    consO(focus, rest0, stackIn),
    selectO(candidateA, rest0, rest1),
    selectO(candidateB, rest1, rest2),
    composeTernaryDirectedO(candidateA, focus, candidateB, focusNew),
    consO(focusNew, rest2, stackOut)
  ))

  private def connectO(stackIn:Term, stackOut:Term, connectName:Term) : Goal = conde(
    (c0_O(stackIn, stackOut), connectName === "c0"),
    (c1_O(stackIn, stackOut), connectName === "c1"),
    (c2_O(stackIn, stackOut), connectName === "c2"),
    (c3_O(stackIn, stackOut), connectName === "c3"),
  )

  def transition_predictConnect_O(stackIn:Term, stackOut:Term, transName:Term) : Goal = fresh( (predName, stackIn2, connectName) => (
    transition_predict_O(stackIn, stackIn2, predName),
    connectO(stackIn2, stackOut, connectName),
    consO(predName, connectName, transName)
  ))

  def transition_predict_O(stackIn:Term, stackOut:Term, transName:Term) : Goal = fresh( (focus, focusNew, rest0) => (
    consO(focus, rest0, stackIn),
    lcPredictO(focus, focusNew, transName),
    consO(focusNew, rest0, stackOut)
  ))

  def transition_shiftConnect_O(stackIn:Term, newEntry:Term, stackOut:Term, transName:Term) : Goal = fresh( (predName, stackIn2, connectName) => (
    transition_shift_O(stackIn, newEntry, stackIn2, predName),
    connectO(stackIn2, stackOut, connectName),
    consO(predName, connectName, transName)
  ))

  def transition_shift_O(stackIn:Term, newEntry:Term, stackOut:Term, transName:Term) : Goal =
    consO(newEntry, stackIn, stackOut) && transName === "shift"

  def lcPredictO(itemIn:Term, itemOut:Term, predictName:Term) : Goal = fresh((mgOpName, moveOpNames, sisterItem, parentItem1, parentItem2) =>
    if(eagerMove){
      (
        mgItemO(itemIn),
        mergeO(itemIn, sisterItem, parentItem1, mgOpName),
        moveTransitiveO(parentItem1, parentItem2, moveOpNames, 1),
        slashItemO(sisterItem, parentItem2, itemOut),
        predictName === list("lcMerge", mgOpName, moveOpNames))
    }else{
      conde(
        (
          mgItemO(itemIn),
          mergeO(itemIn, sisterItem, parentItem1, mgOpName),
          slashItemO(sisterItem, parentItem1, itemOut),
          predictName === list("lcMerge", mgOpName)),
        (
          mgItemO(itemIn),
          moveO(itemIn, itemOut, mgOpName),
          predictName === list("lcMove", mgOpName)))
    }
  )

  private def combineMoversO(movers1:Term, movers2:Term, moversR:Term) : Goal = conde(
    (nullO(movers1), nullO(movers2), nullO(moversR)),
    (fresh((h1, t1, h2, t2, hR, tR) => (
      consO(h1, t1, movers1),
      consO(h2, t2, movers2),
      consO(hR, tR, moversR),
      project(h1, h2){  // this is a check for SMC with some extra-logical black magic to prevent multiple successful unifications when both are NO_MOVER
        case (list(_, Val(NO_MOVER)), list(_, Val(NO_MOVER))) => hR === h1
        case (cons(_, _            ), list(_, Val(NO_MOVER))) => hR === h1
        case (list(_, Val(NO_MOVER)), cons(_, _            )) => hR === h2
        case (list(_, m1:Var       ), cons(_, m2:Var       )) => (m1 === NO_MOVER && m2 === hR) || (m1 === hR && m2 === NO_MOVER)
        case (cons(_, _            ), cons(_, _            )) => FAIL
      },
//      conde(
//        (h1 === cons(__, cons(__, cons(__, __))), h2 === cons(__, cons(__, cons(__, __))), FAIL),  // Implicit check for SMC 1
//        (h1 === list(__, NO_MOVER), h2 === hR), // Implicit check for SMC 1
//        (h2 === list(__, NO_MOVER), h1 === hR)  // Implicit check for SMC 2
//      ),
//      conda(
//        cond(h1 === list(__, NO_MOVER), h2 === hR), // Implicit check for SMC 1
//        cond(h2 === list(__, NO_MOVER), h1 === hR)  // Implicit check for SMC 2
//      ),
      combineMoversO(t1, t2, tR)
    )))
  )

  private def addMover(features:Term, start:Term, end:Term, moversIn:Term, moversOut:Term) : Goal = fresh( f => (
    carO(list(Licensee, f), features),
    replaceO(list(f, NO_MOVER), list(f, start, end, features), moversIn, moversOut) // Implicit check for SMC 3
  ))

  private def mainFeatureO(f:Term, item:Term) : Goal =
    mgItemO(item, __, __, __, cons(f, __), __, __, __)

  private def findHeadAndArgO(item1:Term, item2:Term, itemHead:Term, itemArg:Term) : Goal = conde(
    (
      mainFeatureO(list(Selector, __), item1),
      mainFeatureO(list(Selectee, __), item2),
      itemHead === item1,
      itemArg  === item2,
    ),
    (
      mainFeatureO(list(Selectee, __), item1),
      mainFeatureO(list(Selector, __), item2),
      itemArg  === item1,
      itemHead === item2,
    )
  )

  def moveTransitiveO(itemIn:Term, itemOut:Term, opName:Term, eagerDepth:Int) : Goal = fresh( (itemInMoved, moveType, otherMoveTypes) =>
    conde(
      (
        mainFeatureO(list(Selectee, __), itemIn),
        itemOut === itemIn,
        opName  === list()
      ),
      (
        mainFeatureO(list(Selector, __), itemIn),
        itemOut === itemIn,
        opName  === list()
      ),
      if(eagerDepth>0)
        (
          mainFeatureO(list(Licensor, __), itemIn),
          moveO(itemIn, itemInMoved, moveType),
          moveTransitiveO(itemInMoved, itemOut, otherMoveTypes, eagerDepth-1),
          consO(moveType, otherMoveTypes, opName)
        )
      else
        FAIL
    )
  )

  def moveO(itemIn:Term, itemOut:Term, moveType:Term) : Goal = fresh( (treeIn, argVarsIn, argVarsInTail, startMainIn, startMainOut, endMain, f, fRest, moversIn, moversOut, moverStart, moverEnd, moverFeatsExtra) => (
    mgItemO(itemIn, startMainIn, endMain, ":", cons(list(Licensor, f), fRest), moversIn, treeIn, argVarsIn),
    memberO(list(f, moverStart, moverEnd, cons(list(Licensee, f), moverFeatsExtra)), moversIn),
    conde(
      ( // move 1
        moveType === "move1",
        nullO(moverFeatsExtra),
        startMainIn === moverEnd,
        startMainOut === moverStart,
        replaceO(cons(f, __), list(f, NO_MOVER), moversIn, moversOut)
      ),
      ( // move 2
        moveType === "move2",
        carO(__, moverFeatsExtra),
        startMainOut === startMainIn,
        replaceO(cons(f, __), list(f, moverStart, moverEnd, moverFeatsExtra), moversIn, moversOut)
      )
    ),
    argVarsIn === listImproper(list(__, moveType), argVarsInTail),
    mgItemO(itemOut, startMainOut, endMain, ":", fRest, moversOut, treeIn, argVarsInTail),
  ))

  def mergeO(item1:Term, item2:Term, result:Term, mergeType:Term) : Goal = fresh( (headItem, argItem) => (
    findHeadAndArgO(item1, item2, headItem, argItem),
    mergeOrderedO(headItem, argItem, result, mergeType)
  ))

  private def mergeOrderedO(headItem:Term, argItem:Term, resultItem:Term, mergeType:Term) : Goal = fresh((hTree, hArgVars, hArgVarsTail, aTree, hStart, hEnd, hType, aStart, aEnd, rStart, rEnd, f, hFRest, aFRest, hMovers, aMovers, rMovers1, rMovers2) => (
    mgItemO(headItem, hStart, hEnd, hType, cons(list(Selector, f), hFRest), hMovers, hTree, hArgVars),
    mgItemO(argItem , aStart, aEnd, __, cons(list(Selectee, f), aFRest), aMovers, aTree, list()),
    combineMoversO(hMovers, aMovers, rMovers1),
    conde(
      ( // merge 1
        mergeType === "merge1",
        hType === "::",
        nullO(aFRest),
        rMovers2 === rMovers1,
        rStart   === hStart,
        hEnd     === aStart,
        rEnd     === aEnd
      ),
      ( // merge 2
        mergeType === "merge2",
        hType === ":",
        nullO(aFRest),
        rMovers2 === rMovers1,
        rStart   === aStart,
        rEnd     === hEnd,
        aEnd     === hStart
      ),
      ( // merge 3
        mergeType === "merge3",
        rStart === hStart,
        rEnd === hEnd,
        addMover(aFRest, aStart, aEnd, rMovers1, rMovers2)
      )
    ),
    hArgVars === listImproper(list(aTree, mergeType), hArgVarsTail),
    mgItemO(
      resultItem,
      rStart,
      rEnd,
      ":",
      hFRest,
      rMovers2,
      hTree,
      hArgVarsTail
    )
  ))

}
