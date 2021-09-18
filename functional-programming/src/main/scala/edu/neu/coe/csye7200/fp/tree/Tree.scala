package edu.neu.coe.csye7200.fp.tree

import scala.annotation.tailrec

/**
  * This trait expresses the notion of a Node from a tree.
  *
  * Created by scalaprof on 10/19/16.
  */
sealed trait Node[+A] {
  /**
    * Optionally get the value of this Node.
    *
    * @return If this node contains an actual value x, return Some(x) else None
    */
  def get: Option[A]

  /**
    * Get the children of this Node, if any.
    *
    * @return the children as a Seq of Nodes
    */
  def children: Seq[Node[A]]

  /**
    * Calculate the size of this node's subtree
    *
    * @return the size of the subtree
    */
  def size: Int

  /**
    * Calculate the depth of this node's subtree
    *
    * @return the depth of the subtree
    */
  def depth: Int

  /**
    * Test whether this Node contains a value or children
    *
    * @return true if node contains value or children; otherwise false
    */
  def isEmpty: Boolean

  /**
    * Create a String which represents this Node and its subtree
    *
    * @return an appropriate String
    */
  def render: String

  /**
    * CONSIDER moving this into TreeLike
    *
    * @param b the value to be searched
    * @tparam B the type of b
    * @return true if value b is found in the sub-tree defined by this
    */
  def includes[B >: A](b: B): Boolean
}


object Node {
  /**
    * Implicit definition of a Parent for the (type-parameterized, i.e. generic) type Node[A]
    *
    * @tparam A the underlying node type
    * @return a Parent of type Node[A]
    */
  implicit def nodeParent[A]: Parent[Node[A]] = (t: Node[A]) => t.children
}

/**
  * This trait expresses the notion of a tree-like structure.
  *
  * Created by scalaprof on 10/19/16.
  */
sealed trait TreeLike[+A] extends Node[A] {

  /**
    * Method to safely cast Node n to a TreeLike object.
    * Note: This is not an instance method in that it does not reference this
    *
    * @param n         the node to cast
    * @param treeMaker implicit implementation of TreeMaker trait
    * @tparam B the underlying type of node n
    * @return if n is already a TreeLike object then return it, otherwise return a sub-tree based on n
    */
  def asTree[B >: A : Ordering](n: Node[B])(implicit treeMaker: TreeMaker): TreeLike[B] = n match {
    case t: TreeLike[B] => t
    case _ => treeMaker.tree(n)
  }

  /**
    * Method to combine this tree with Node b (where the tree is on the left of the operator)
    *
    * @param b the node to be added to this tree
    * @tparam B the underlying type of b
    * @return a new subtree of type B, as a Node[B]
    */
  def :+[B >: A : Ordering](b: Node[B]): Node[B]

  /**
    * Method to combine this tree with Node b (where the tree is on the right of the operator)
    *
    * @param b         the node to be added to this tree
    * @param treeMaker implicit implementation of TreeMaker trait
    * @tparam B the underlying type of node n
    * @return if n is already a TreeLike object then return it, otherwise return a sub-tree based on n
    */
  def +:[B >: A : Ordering](b: Node[B])(implicit treeMaker: TreeMaker): Node[B] = asTree(b) :+ this

  def size: Int = Parent.traverse[Node[A], Int]({
    case Empty => 0
    case _ => 1
  }, _ + _, { _ => false }, _ ++ _.children)(List(this), 0)

  def includes[B >: A](b: B): Boolean = Parent.traverse[Node[B], Boolean]({ n =>
    n.get match {
      case Some(`b`) => true
      case _ => false
    }
  }, _ | _, { x => x }, _ ++ _.children)(List(this), false)

  def renderRecursive[B >: A](z: (List[Node[B]], Node[B]) => List[Node[B]]): String =
    Parent.traverse[Node[B], String]({
      case Empty => ""
      case Leaf(x) => x.toString
      case c@Open => c.render
      case c@Close => c.render
      case _ => ""
    }, _ + _, { _ => false }, z)(List(this), "")

  override def render: String = renderRecursive[A]((ns, n) => n.children.toList ++ ns)
}

/**
  * This trait expresses the notion of a tree-like structure that has properties depth and size.
  *
  * Created by scalaprof on 10/19/16.
  */
sealed abstract class Tree[+A] extends TreeLike[A] {
  /**
    * NOTE: that this is NOT tail-recursive
    *
    * NOTE: I don't think we can use Parent.traverse for depth. Could be wrong.
    *
    * @return the depth of the subtree
    */
  def depth: Int = 1 + children.map(_.depth).max
}

trait TreeMaker {
  def tree[T](node: Node[T]): TreeLike[T]
}

case class Leaf[+A](value: A)(implicit treeMaker: TreeMaker) extends TreeLike[A] {
  def children: Seq[Node[A]] = Nil

  def get = Some(value)

  def :+[B >: A : Ordering](b: Node[B]): Node[B] = treeMaker.tree(b) :+ this

  override def depth: Int = 1

  //  override def render = value.toString
  def isEmpty: Boolean = false
}

case object Empty extends TreeLike[Nothing] {
  def children: Seq[Node[Nothing]] = Nil

  def get: None.type = None

  def :+[B >: Nothing : Ordering](bn: Node[B]): Node[B] = bn

  override def depth: Int = 0

  //  override def render = ""
  def isEmpty: Boolean = true
}

/**
  * This abstract class knows how to create a binary tree
  *
  * @param value the value at the root of this abstract binary tree
  * @param left  the tree to the "left" (the tree which contains values which are less than value)
  * @param right the tree to the "right"  (the tree which contains values which are greater than value)
  * @tparam A the underlying type
  */
sealed abstract class AbstractBinaryTree[+A](value: A, left: Node[A], right: Node[A]) extends Tree[A] {
  def get: Option[A] = Some(value)

  def children: Seq[Node[A]] = Seq(left, right)

  implicit val builder: TreeMaker = new TreeMaker {
    def tree[T](node: Node[T]): TreeLike[T] = make(node.get.get, Empty, Empty)
  }

  def :+[B >: A : Ordering](n: Node[B]): Node[B] = {
    val b = implicitly[Ordering[B]].compare(value, n.get.get) < 0

    def maybeAdd(x: Node[B], y: Boolean): Node[B] = if (y) asTree(x) :+ n else x

    make(value, maybeAdd(left, !b), maybeAdd(right, b))
  }

  def make[T](x: T, l: Node[T], r: Node[T]): AbstractBinaryTree[T]

  def isEmpty: Boolean = false

  override def render: String = renderRecursive[A](AbstractBinaryTree.buildNodeList)
}

object AbstractBinaryTree {
  def buildNodeList[T](nodes: List[Node[T]], node: Node[T]): List[Node[T]] = node match {
    case BinaryTree(x, l, r) => (expand(l) :+ Leaf(x)(node.asInstanceOf[BinaryTree[T]].builder)) ++ expand(r) ++ nodes
    case _ => node.children.toList ++ nodes
  }

  private[tree] def expand[T](node: Node[T]): List[Node[T]] = node match {
    case Empty => Nil
    case n => List(Open, n, Close)
  }
}

/**
  * This implements a non-indexed binary tree
  *
  * @param value the value at the root of this abstract binary tree
  * @param left  the tree to the "left" (the tree which contains values which are less than value)
  * @param right the tree to the "right"  (the tree which contains values which are greater than value)
  * @tparam A the underlying type
  */
case class BinaryTree[+A](value: A, left: Node[A], right: Node[A]) extends AbstractBinaryTree[A](value, left, right) {
  def make[T](x: T, l: Node[T], r: Node[T]): AbstractBinaryTree[T] = BinaryTree(x, l, r)
}

/**
  * This implementation of Node supports MPTT (Modified pre-order tree traverse).
  * Thus it is very expensive to modify the tree. But very fast to make sub-tree queries.
  *
  * @param leftIndex  the value of the index to the left of the tree
  * @param rightIndex the value of the index to the right of the tree
  * @param value      the value at the root of this abstract binary tree
  * @param left       the tree to the "left" (the tree which contains values which are less than value)
  * @param right      the tree to the "right"  (the tree which contains values which are greater than value)
  * @tparam A the underlying type
  */
case class IndexedTree[+A](leftIndex: Int, rightIndex: Int, value: A, left: Node[A], right: Node[A]) extends AbstractBinaryTree[A](value, left, right) with BinaryTreeIndex {
  def make[T](x: T, l: Node[T], r: Node[T]): AbstractBinaryTree[T] = {
    val (_, t) = IndexedTree.makeIndexedTree(0, x, l, r)
    t.asInstanceOf[AbstractBinaryTree[T]]
  }
}

/**
  * Indexed version of Leaf.
  * CONSIDER: Could extend Leaf
  *
  * @param leftIndex  the left index of this leaf node
  * @param rightIndex the right index of this leaf node
  * @param value      the value of this leaf node
  * @param treeMaker  an implementer of TreeMaker
  * @tparam A the underlying node type
  */
case class IndexedLeaf[+A](leftIndex: Int, rightIndex: Int, value: A)(implicit treeMaker: TreeMaker) extends TreeLike[A] with BinaryTreeIndex {
  def children: Seq[Node[A]] = Nil

  def get = Some(value)

  def :+[B >: A : Ordering](b: Node[B]): Node[B] = treeMaker.tree(b) :+ this

  override def depth: Int = 1

  def isEmpty: Boolean = false
}

object IndexedTree {
  /**
    * Method to create an indexed subtree from the given details.
    *
    * Note: this is recursive but NOT tail-recursive
    *
    * @param index the left-index that will be appropriate for this subtree
    * @param x     the value at the root of the subtree
    * @param l     the left tree
    * @param r     the right tree
    * @tparam T the underlying type of the tree
    * @return a tuple consisting of the new index value and the indexed subtree
    */
  def makeIndexedTree[T](index: Int, x: T, l: Node[T], r: Node[T]): (Int, Node[T]) = {
    println(s"makeIndexedTree (1): $index, $x, $l, $r")
    val (i, li) = indexSubtree(index, l)
    println(s"makeIndexedTree (2): $i, $li")
    val (j, ri) = indexSubtree(i + 2, r)
    println(s"makeIndexedTree (3): $j, $ri")
    (j, IndexedTree(index, j, x, li, ri))
  }

  private[tree] def indexSubtree[T](index: Int, node: Node[T]): (Int, Node[T]) = node match {
    case BinaryTree(v, l, r) => makeIndexedTree(index, v, l, r)
    case IndexedTree(_, _, v, l, r) => makeIndexedTree(index, v, l, r)
    // TODO fix this    case Leaf(v) => (index+2,IndexedLeaf(index,index+1,v))
    case Empty => (index, node)
    case _ => throw TreeException(s"illegal node in IndexedTree: $node")
  }

  implicit val treeMaker: TreeMaker = new TreeMaker {
    def tree[T](node: Node[T]): TreeLike[T] = indexSubtree(0, node)._2.asInstanceOf[TreeLike[T]]
  }
}

case class UnsortedTree[+A](value: A, children: Seq[Node[A]]) extends Tree[A] {
  def get = Some(value)

  def :+[B >: A : Ordering](bn: Node[B]): Node[B] = UnsortedTree(value, children :+ bn)

  def isEmpty: Boolean = false
}

object UnsortedTree {
  def apply[A](as: A*): Node[A] = as.toList match {
    case Nil => Empty.asInstanceOf[Node[A]]
    case h :: t =>
      apply(h, List(apply(t: _*)))
  }

  def apply[A](as: List[A]): Node[A] = apply(as: _*)
}

object BinaryTree {
  def apply[A: Ordering](as: A*): BinaryTree[A] = as.toList match {
    case Nil => Empty.asInstanceOf[BinaryTree[A]]
    case h :: Nil => BinaryTree(h, Empty, Empty)
    case h :: t => (apply(h) :+ apply(t: _*)).asInstanceOf[BinaryTree[A]]
  }

  implicit val treeMaker: TreeMaker = new TreeMaker {
    @tailrec
    def tree[T](node: Node[T]): TreeLike[T] = node match {
      case t: BinaryTree[T] => t
      case _ => tree(node) // FIXME this looks recursive
    }
  }
}

case class TreeException(msg: String) extends Exception(msg)

object Tree {
  def join(xo: Option[String], yo: Option[String]): Option[String] = {
    val mt = Some("")
    for (x <- xo.orElse(mt); y <- yo.orElse(mt)) yield x + y
  }
}

trait BinaryTreeIndex {
  def leftIndex: Int

  def rightIndex: Int
}

abstract class Punctuation(x: String) extends Node[Nothing] {
  def get: Option[Nothing] = None

  def children: Seq[Node[Nothing]] = Nil

  def includes[B >: Nothing](b: B): Boolean = false

  def size: Int = 0

  def render: String = x

  def isEmpty: Boolean = true

  def depth: Int = 0
}

case object Open extends Punctuation("{")

case object Close extends Punctuation("}")
