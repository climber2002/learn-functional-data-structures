package com.functional.ch04

object Greedy {
  def greedyChange(demon: List[Int], amount: Int): List[Int] = (demon, amount) match {
    case (_, 0) => Nil
    case (x :: xs, _) if amount < x => greedyChange(xs, amount)
    case (x :: _, _) => x :: greedyChange(demon, amount - x)
  }
}
