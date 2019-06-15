package com.functional.ch03

sealed trait BinTree[+A]

case object Leaf extends BinTree[Nothing]

case class Branch[A](value: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]

object BinTree {
  def buildTree[A](list: List[A]): BinTree[A] = list match {
    case Nil => Leaf
    case x :: xs => {
      val k = xs.length / 2
      Branch(x, buildTree(xs.take(k)), buildTree(xs.drop(k)))
    }
  }
}
