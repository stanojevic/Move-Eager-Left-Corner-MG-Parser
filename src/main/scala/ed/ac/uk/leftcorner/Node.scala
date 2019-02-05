package ed.ac.uk.leftcorner

import java.io.File

import ed.ac.uk.visualization.TreeVisualizer.{Color, Shape, SimpleTreeNode}
import ed.ac.uk.kanren.Kanren.Term
import ed.ac.uk.kanren.{KList, Val}

sealed trait Node{

  def visualize(graphLabel:String="", fileType:String="pdf") : Unit =
    toSimpleNode.visualize(graphLabel=graphLabel, fileType=fileType)

  def saveVisual(fn:String, graphLabel:String="", fileType:String="pdf") : Unit =
    toSimpleNode.saveVisual(outFile = new File(fn+"."+fileType), graphLabel=graphLabel, fileType=fileType)

  private def toSimpleNode : SimpleTreeNode = this match {
    case MoveNode(moveType, child) =>
      SimpleTreeNode(label=moveType, children = List(child.toSimpleNode), color = Color.GRAY)
    case MergeNode(mergeType, left, right) =>
      SimpleTreeNode(label=mergeType, children = List(left.toSimpleNode, right.toSimpleNode), color = Color.BLACK)
    case TerminalNode(start, end, word, feats) =>
      val w = if(word == "") "ε" else word
      SimpleTreeNode(label=s"$feats\n$word", children = List(), position = start*100+end, shape=Shape.RECTANGLE)
  }

  override def toString: String = Node.recToString(this)

}

case class MoveNode(moveType:String, child:Node) extends Node
case class MergeNode(mergeType:String, left:Node, right:Node) extends Node
case class TerminalNode(start:Int, end:Int, word:String, feats:String) extends Node


object Node{

  def fromMgItem(mgItem:Term) : Node = {
    recTransform(mgItem.asKList(5))
  }

  def recTransform(term: Term) : Node = term match {
    case KList(Val(moveType:String), child) => MoveNode(moveType, recTransform(child))
    case KList(Val(mergeType:String), left, right) => MergeNode(mergeType, recTransform(left), recTransform(right))
    case KList(Val(start:Int), Val(end:Int), Val(word:String), Val(feats:String)) => TerminalNode(start, end, word, feats)
  }

  private def recToString(node:Node, depth:Int = 0) : String = {
    val prefix = ("\t"*depth)+"("
    val (infix, suffix) = node match {
      case MoveNode(moveType, child) =>
        (
          moveType+"\n"+recToString(child, depth+1),
          ("\t"*depth)+")\n"
        )
      case MergeNode(mergeType, left, right) =>
        (
          mergeType+"\n"+recToString(left, depth+1)+recToString(right, depth+1),
          ("\t"*depth)+")\n"
        )
      case TerminalNode(start, end, word, feats) =>
        (
          s"$word :: $feats",
          ")\n"
        )
    }
    prefix+infix+suffix
  }

}


