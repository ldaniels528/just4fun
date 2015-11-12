package com.github.ldaniels528.just4fun

import scala.collection.mutable

/**
 * Sorting Algorithms
 * @author lawrence.daniels@gmail.com
 */
object Sorting {

  def main(args: Array[String]) = {
    val array = Array(2, 0, 1, 0, 1, 2, 1, 1, 0, 0, 1)
    System.out.println(s"BEFORE: array = ${array.toSeq}")
    val ops = flagSort(array)
    System.out.println(s"AFTER: array = ${array.toSeq}, ops = $ops")
  }

  def flagSort(array: Array[Int]) = {
    val last = mutable.Map[Int, Int]()
    var index = 1
    var loops = 0

    while (index < array.length) {
      val p0 = index - 1
      val p1 = index
      System.out.println(s"DATA: array = ${dump(array, p0, p1, last)}")

      val a = array(p0)
      val b = array(p1)

      if (a > b) {
        last.get(b) map(_ + 1) match {
          case Some(p2) =>
            System.out.println(s"DBG1: array = ${dump(array, p0, p1, last)}")

            val aa = array(p2)
            array(p2) = b
            array(p1) = aa

            last(aa) = p1
            last(b) = p2
          case None =>
            array(p0) = b
            array(p1) = a
            index -= 1

            last(a) = p1
            last(b) = p0
            System.out.println(s"DBG2: array = ${dump(array, p0, p1, last)}")
        }
      }
      else {
        last(a) = p0
        last(b) = p1
      }

      index += 1
      loops += 1
    }
    loops
  }

  def dump[T](array: Array[T], anchor: T, pivot: T, last: mutable.Map[T, Int]) = {
    val data = array.indices zip array map { case (pos, value) =>
      pos match {
        case n if n == anchor => s"($value)"
        case n if n == pivot => s"[$value]"
        case n => s"$value"
      }
    } mkString " "
    s"$data - last = $last"
  }

}
