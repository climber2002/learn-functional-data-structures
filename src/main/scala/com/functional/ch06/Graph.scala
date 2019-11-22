package com.functional.ch06

//val graph = List(("m", "n"), ("m", "o"), ("m", "p"),
//             ("n", "q"), ("o", "r"), ("p", "q"),
//             ("q", "r"), ("q", "s"))
object Graph {
  def succSet(a: String, g: List[(String, String)]): List[String] = g match {
    case Nil => Nil
    case x :: xs if a == x._1 => x._2 :: succSet(a, xs)
    case _ :: xs => succSet(a, xs)
  }

  def depthFirst(initial: String, g: List[(String, String)]) : List[String] = {
    def depthf(nodes: List[String], visited: List[String]): List[String] = nodes match {
      case Nil => visited
      case x :: xs if visited.contains(x) => depthf(xs, visited)
      case x :: xs => depthf(succSet(x, g) ++ xs, x :: visited)
    }

    val result = depthf(List(initial), List())
    result.reverse
  }

//  val grwork = List(("getup","shower"),
//    ("shower", "breakfast"),
//    ("breakfast","dress"),
//    ("dress","office"),
//    ("office", "dinner"),
//    ("breakfast","leisurely_lunch"),
//    ("leisurely_lunch", "movie"),
//    ("movie", "dinner"))

  def topsort(g: List[(String, String)]) = {
    def sort(nodes: List[String], visited: List[String]): List[String] = nodes match {
      case Nil => visited
      case x :: xs => sort(xs,
        if(visited.contains(x)) visited
        else x :: sort(succSet(x, g), visited))
    }

    val (start, _) = g.unzip
    sort(start, List())
  }
}
