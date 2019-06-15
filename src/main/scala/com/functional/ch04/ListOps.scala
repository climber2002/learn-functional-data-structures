package com.functional.ch04

object ListOps {
  def carry(c: Int, list: List[Int]):List[Int] = (c, list) match {
    case (0, _) => list
    case (1, Nil) => List(1)
    case (1, 0 :: tail) => 1 :: tail
    case (1, 1 :: tail) => 0 :: carry(1, tail)
    case (_, _) => throw new IllegalArgumentException("Invalid input!!!")
  }

  def add(c: Int, ps: List[Int], qs: List[Int]): List[Int] = (ps, qs) match {
    case (Nil, Nil) => carry(c, Nil)
    case (Nil, _ :: _) => carry(c, qs)
    case (_ :: _, Nil) => carry(c, ps)
    case (x :: xs, y :: ys) => ((c + x + y) % 2) :: add((c + x + y) / 2, xs, ys)
  }

  def addNums(first: List[Int], second: List[Int]) : List[Int] = {
    val result = add(0, first.reverse, second.reverse)
    result.reverse
  }

  def mult(first: List[Int], second: List[Int]): List[Int] = {
    def multiply(ps: List[Int], qs: List[Int]): List[Int] = ps match {
      case Nil => Nil
      case 0 :: xs => 0 :: multiply(xs, qs)
      case 1 :: xs => add(0, qs, 0 :: multiply(xs, qs))
    }

    val result = multiply(first.reverse, second.reverse)
    result.reverse
  }

//  def mult2(first: List[Int], second: List[Int]): List[Int] = {
//    def multiply(ps: List[Int], qs: List[Int]): List[Int] = ps match {
//      case Nil => Nil
//      case 0 :: xs => 0 :: multiply(xs, qs)
//      case 1 :: xs => add(0, qs, multiply(0 :: xs, qs))
//    }
//
//    val result = multiply(first.reverse, second.reverse)
//    result.reverse
//  }

}
