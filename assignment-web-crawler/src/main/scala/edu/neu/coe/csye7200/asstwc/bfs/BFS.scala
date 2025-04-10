//package edu.neu.coe.csye7200.asstwc.bfs
//
//import scala.collection.immutable.Queue
//
//trait BFS[+X] {
//  def goal[Y >: X](node: Node[Y]): Option[Y]
//  def bfs[Y >: X](q: Queue[Node[Y]]): (Seq[Node[Y]], Queue[Node[Y]])
//}
//
//trait Node[+X]  {
//  def x: X
//  def children: Seq[Node[X]]
//}
//
//trait BuddingNode[X] extends Node[X] {
//  def addChild(node: Node[X]): Unit
//}
//
//case class Tree[+X](x: X, children: Seq[Node[X]]) extends Node[X]
//
//object Tree {
//  def apply[X](x: X, children: Node[X]*): Tree[X] = Tree(x, children)
//}
//
//object BFS {
//
//}