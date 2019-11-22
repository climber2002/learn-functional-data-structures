package com.functional.ch04

object Greedy {
  def greedyChange(demon: List[Int], amount: Int): List[Int] = (demon, amount) match {
    case (_, 0) => Nil
    case (x :: xs, _) if amount < x => greedyChange(xs, amount)
    case (x :: _, _) => x :: greedyChange(demon, amount - x)
  }

  def btChanges(result: List[Int], denom: List[Int], amount: Int): List[List[Int]] =
    if (amount == 0)
      List(result)
    else
      denom match {
        case Nil => List()
        case x :: xs if amount < 0 => List()
        case x :: xs => btChanges(x::result, denom, amount - x) ++
          btChanges(result, xs, amount)
      }
}
