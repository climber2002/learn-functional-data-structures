package com.functional.ch02

sealed trait List[+A]

case object Nil extends List[Nothing]
case class ::[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else ::(as.head, apply(as.tail: _*))

  def head[A](list: List[A]): A = list match {
    case Nil => sys.error("head of empty list")
    case (a :: _) => a
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("tail of empty list")
    case (_ :: xs) => xs
  }

  def drop[A](list: List[A], n: Int): List[A] =
    if(n <= 0) list
    else list match {
      case Nil => Nil
      case (_ :: tail) => drop(tail, n - 1)
    }

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case x :: xs if f(x) => dropWhile(xs, f)
    case _ => list
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case (a :: as) => ::(a, append(as, a2))
  }

  def appendElem[A](l: List[A], elem: A): List[A] = l match {
    case Nil => List(elem)
    case (x :: xs) => ::(x, appendElem(xs, elem))
  }

  def prependElem[A](l: List[A], elem: A): List[A] = ::(elem, l)

  def elemAtIndex[A](l: List[A], i: Int): A = (l, i) match {
    case (Nil, _) => sys.error(s"index ${i} not valid")
    case (x :: xs, 0) => x
    case (x :: xs, _) => elemAtIndex(xs, i - 1)
  }

  def setElem[A](l: List[A], i: Int, elem: A): List[A] = (l, i) match {
    case (Nil, _) => sys.error(s"index ${i} not valid")
    case (x :: xs, 0) => ::(elem, xs)
    case (x :: xs, i) => ::(x, setElem(xs, i - 1, elem))
  }
}