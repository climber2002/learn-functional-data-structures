package com.functional.ch06

object ListOpts {
  def rev(list: List[Int], acc: List[Int]): List[Int] = list match {
    case Nil => acc
    case x :: xs => rev(xs, x :: acc)
  }
}
