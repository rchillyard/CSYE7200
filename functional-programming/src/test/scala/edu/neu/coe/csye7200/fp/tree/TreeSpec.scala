package edu.neu.coe.csye7200.fp.tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Created by scalaprof on 10/19/16.
  */
class TreeSpec extends AnyFlatSpec with Matchers {

  behavior of "UnsortedTree"
  it should "apply correctly with varargs" in {
    val tree1 = UnsortedTree(1, Seq(UnsortedTree(2, Seq(UnsortedTree(3, Seq(Empty))))))
    val tree2 = UnsortedTree(1, 2, 3)
    tree1 shouldBe tree2
  }
  it should "apply correctly with List" in {
    val tree1 = UnsortedTree(1, Seq(UnsortedTree(2, Seq(UnsortedTree(3, Seq(Empty))))))
    val tree2 = UnsortedTree(1, 2, 3)
    tree1 shouldBe tree2
  }

  behavior of "BinaryTree"
  it should "apply correctly with varargs" in {
    val tree = BinaryTree(1, 2, 3)
    tree.includes(1) shouldBe true
    tree.depth shouldBe 3
    tree.render shouldBe "1{2{3}}"
  }
  it should "apply correctly with List" in {
    val tree1 = UnsortedTree(1, Seq(UnsortedTree(2, Seq(UnsortedTree(3, Seq(Empty))))))
    val tree2 = UnsortedTree(1, 2, 3)
    tree1 shouldBe tree2
  }

  behavior of ":+"
  it should "work" in {
    import BinaryTree._
    val x = BinaryTree[String]("A", "C", "D")
    val y = x :+ Leaf("B")
    y.size shouldBe 4
    y.depth shouldBe 3
    y.render shouldBe "A{{B}C{D}}"
  }

  behavior of "get"
  it should "work" in {
    implicit val builder: TreeMaker = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = UnsortedTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = UnsortedTree(1, Seq(Leaf(2), Leaf(3)))
    tree.includes(1) shouldBe true
    tree.includes(2) shouldBe true
    tree.includes(3) shouldBe true
    tree.includes(4) shouldBe false
    tree.includes(0) shouldBe false
  }

  behavior of "depth"
  it should "work for UnsortedTree" in {
    implicit val builder: TreeMaker = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = UnsortedTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = UnsortedTree(1, Seq(Leaf(2), Leaf(3)))
    tree.depth shouldBe 2
  }
  it should "work for BinaryTree" in {
    val tree: Node[Int] = BinaryTree(1, 2, 3)
    tree.depth shouldBe 3
    val tree2 = tree.asInstanceOf[BinaryTree[Int]] :+ BinaryTree(1)
    tree2.asInstanceOf[BinaryTree[Int]].depth shouldBe 3
  }

  behavior of "size"
  it should "work for UnsortedTree" in {
    implicit val builder: TreeMaker = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = UnsortedTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = UnsortedTree(1, Seq(Leaf(2), Leaf(3)))
    tree.size shouldBe 3
  }
  it should "work for BinaryTree" in {
    val tree: Node[Int] = BinaryTree(1, 2, 3)
    tree.size shouldBe 3
    val tree2 = tree.asInstanceOf[BinaryTree[Int]] :+ BinaryTree(1)
    tree2.asInstanceOf[BinaryTree[Int]].size shouldBe 4
  }

  behavior of "includes"
  it should "work for UnsortedTree" in {
    implicit val builder: TreeMaker = new TreeMaker {
      def tree[A](node: Node[A]): TreeLike[A] = UnsortedTree(node.get.get).asInstanceOf[TreeLike[A]]
    }
    val tree = UnsortedTree(1, Seq(Leaf(2), Leaf(3)))
    tree.includes(1) shouldBe true
    tree.includes(2) shouldBe true
    tree.includes(3) shouldBe true
    tree.includes(4) shouldBe false
  }
  it should "work for BinaryTree" in {
    val tree: Node[Int] = BinaryTree(1, 2, 3)
    tree.includes(1) shouldBe true
    tree.includes(2) shouldBe true
    tree.includes(3) shouldBe true
    tree.includes(4) shouldBe false
  }

  //  behavior of "IndexedTree"
  //  it should "work for one element" in {
  //    val tree: Node[String] = BinaryTree("A")
  //    val (size,x) = IndexedTree.indexSubtree(0,tree)
  //    println(s"x: $p")
  //    size shouldBe 1
  //    x.asInstanceOf[IndexedTree[String]].leftIndex shouldBe 0
  //    x.asInstanceOf[IndexedTree[String]].rightIndex shouldBe 2
  //  }
  //  it should "work for three elementa" in {
  //    val tree: Node[String] = BinaryTree("A","B","C")
  //    val (size,x) = IndexedTree.indexSubtree(0,tree)
  //    println(s"x: $p")
  //    size shouldBe 3
  //    x.asInstanceOf[IndexedTree[String]].leftIndex shouldBe 0
  //    x.asInstanceOf[IndexedTree[String]].rightIndex shouldBe 3
  //  }
  //  it should "work for IndexedTree with extra node" in {
  //    import BinaryTree._
  //    val tree: AbstractBinaryTree[String] = BinaryTree("A","B","C").asInstanceOf[AbstractBinaryTree[String]]
  //    val tree2 = tree :+ Leaf("Aardvark")
  //    val (size,x) = IndexedTree.indexSubtree(0,tree2)
  //    size shouldBe 4
  //    println(s"x: ${p.render}")
  //    x.asInstanceOf[IndexedTree[String]].leftIndex shouldBe 0
  //    x.asInstanceOf[IndexedTree[String]].rightIndex shouldBe 8
  //  }
}
