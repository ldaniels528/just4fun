package com.github.ldaniels528.just4fun

import com.github.ldaniels528.just4fun.QuickList.QNode

import scala.language.postfixOps

/**
 * A fast mutable Linked List implementation
 * @author lawrence.daniels@gmail.com
 */
class QuickList[T]() {
  private var root: Option[QNode[T]] = None
  private var trunk: Option[QNode[T]] = None
  private var mySize = 0

  def ::(value: T) = append(value)

  def :::(values: Seq[T]) = appendAll(values)

  /**
   * Adds the given value to the end of the list (complexity: O(1))
   * @param value the given value
   */
  def append(value: T) = {
    if (root.isDefined) {
      val node = Option(QNode(value, previous = trunk))
      trunk.foreach(_.next = node)
      trunk = node
    }
    else {
      root = Option(QNode(value))
      trunk = root
    }
    mySize += 1
  }

  /**
   * Appends the given values (complexity: O(m))
   * @param values the given values
   */
  def appendAll(values: Seq[T]) = values foreach append

  /**
   * Counts all elements within the list that satisfy the given expression (complexity: O(n))
   * @param f the given expression
   * @return the number of elements that satisfy the expression
   */
  def count(f: T => Boolean): Int = {
    var count = 0
    var current = root
    while (current.isDefined) {
      if (current.exists(node => f(node.value))) count += 1
      current = current.flatMap(_.next)
    }
    count
  }

  /**
   * Determines if any elements exist within the list that satisfy the given expression (complexity: O(n))
   * @param f the given expression
   * @return true, if any elements satisfy the expression
   */
  def exists(f: T => Boolean): Boolean = {
    var current = root
    while (current.isDefined) {
      if (current.exists(node => f(node.value))) return true
      current = current.flatMap(_.next)
    }
    false
  }

  /**
   * Retrieves the value at the given index (complexity: O(m))
   * @param index the given index
   * @return an option of the value)
   */
  def get(index: Int): Option[T] = {
    var current = root
    for (_ <- 0 to index - 1) current = current.flatMap(_.next)
    current.map(_.value)
  }

  /**
   * Retrieves the first value in the list (complexity: O(1))
   * @return the first value in the list
   */
  def headOption: Option[T] = root.map(_.value)

  /**
   * Returns the index of the given value (complexity: O(n))
   * @param value the given value
   * @return the index of first element that is equal to the given value, or -1
   */
  def indexOf(value: T): Int = indexWhere(_ == value)

  /**
   * Returns the index of the first element that satisfy the given expression (complexity: O(n))
   * @param f the given expression
   * @return the index of first element that satisfy the expression, or -1
   */
  def indexWhere(f: T => Boolean): Int = {
    var index = 0
    var current = root
    while (current.isDefined) {
      if (current.exists(node => f(node.value))) return index
      current = current.flatMap(_.next)
      index += 1
    }
    -1
  }

  def iterator: Iterator[T] = new Iterator[T] {
    var current = root

    override def hasNext: Boolean = current.isDefined

    override def next(): T = {
      val value = current.map(_.value) match {
        case Some(v) => v
        case None =>
          throw new IllegalStateException("Iterator is empty")
      }
      current = current.flatMap(_.next)
      value
    }
  }

  def isEmpty: Boolean = root.isEmpty

  /**
   * Retrieves the last value in the list (complexity: O(1))
   * @return the last value in the list
   */
  def lastOption: Option[T] = trunk.map(_.value)

  def nonEmpty: Boolean = root.nonEmpty

  /**
   * Removes and returns the last value from the list (complexity: O(1))
   * @return the last value from the list
   * @see [[removeLast()]]
   */
  def pop(): Option[T] = removeLast()

  /**
   * Adds the given value to the beginning of the list (complexity: O(1))
   * @param value the given value
   */
  def prepend(value: T) = {
    if (root.isDefined) {
      val node = Option(QNode(value, next = root))
      root.foreach(_.previous = node)
      root = node
      mySize += 1
    }
    else append(value)
  }

  /**
   * Adds the given value to the end of the list (complexity: O(1))
   * @param value the given value
   * @see [[append()]]
   */
  def push(value: T) = append(value)

  /**
   * Removes and returns the first value from the list (complexity: O(1))
   * @return the first value from the list
   */
  def removeFirst(): Option[T] = {
    val (oldRoot, newRoot) = (root, root.flatMap(_.next))
    root = newRoot
    newRoot.foreach(_.previous = None)
    mySize -= 1
    oldRoot.map(_.value)
  }

  /**
   * Removes and returns the last value from the list (complexity: O(1))
   * @return the last value from the list
   */
  def removeLast(): Option[T] = {
    val oldLast = trunk
    val newLast = oldLast.flatMap(_.previous)
    newLast.foreach(_.next = None)
    trunk = newLast
    if (trunk.isDefined) mySize -= 1
    oldLast.map(_.value)
  }

  def reverse: QuickList[T] = {
    val list = new QuickList[T]()
    var current = root
    while (current.isDefined) {
      current.foreach(node => list.prepend(node.value))
      current = current.flatMap(_.next)
    }
    list
  }

  def shift(value: T) = prepend(value)

  /**
   * Returns the count of the number of elements in the list (complexity: O(1))
   * @return the number of elements in the list
   */
  def size: Int = mySize

  /**
   * Updates a value within the list by index
   * @param index the given index
   * @param value the given value
   */
  def update(index: Int, value: T) = {
    var current = root
    for (_ <- 0 to index - 1) current = current.flatMap(_.next)
    current.foreach(_.value = value)
  }

  /**
   * Retrieves all values except the first value from the list (complexity: O(1))
   * @return a list of all values except the first value
   */
  def tail: QuickList[T] = {
    val tailList = new QuickList[T]()
    if (root == trunk) tailList
    else {
      tailList.root = root.flatMap(_.next)
      tailList.trunk = trunk
      tailList.mySize = mySize - 1
      tailList
    }
  }

  /**
   * Retrieves all values from the list (complexity: O(n))
   * @return a list of all values
   */
  def toList: List[T] = {
    var current = root
    var list: List[T] = Nil
    while (current.isDefined) {
      current.foreach(node => list = node.value :: list)
      current = current.flatMap(_.next)
    }
    list.reverse
  }

  /**
   * Retrieves all values from the list (complexity: O(n))
   * @return a sequence of all values
   */
  def toSeq: Seq[T] = toList

  override def toString = {
    val sb = new StringBuilder()
    var current = root
    while (current.isDefined) {
      current.foreach(node => sb.append(", ").append(node.value))
      current = current.flatMap(_.next)
    }
    s"${getClass.getSimpleName}(${if (sb.nonEmpty) sb.substring(2) else ""})"
  }

  /**
   * Removes and returns the first value from the list (complexity: O(1))
   * @return the first value from the list
   * @see [[removeFirst()]]
   */
  def unshift(): Option[T] = removeFirst()

}

/**
 * Quick List Companion
 * @author lawrence.daniels@gmail.com
 */
object QuickList {

  def apply[T](values: T*) = {
    val list = new QuickList[T]()
    list.appendAll(values)
    list
  }

  def main(args: Array[String]) = {
    val list = QuickList[String]("Apple", "Pair", "Orange", "Banana")
    System.out.println(s"list = $list, list.size = ${list.size}")

    list.append("Grape")
    System.out.println(s"list = $list, list.size = ${list.size}")

    list.appendAll(Seq("Pomegranate", "Grapefruit"))
    System.out.println(s"list = $list, list.size = ${list.size}")

    System.out.println( s"""list.count(_.startsWith("G")) = ${list.count(_.startsWith("G"))}""")
    System.out.println( s"""list.indexWhere(_.startsWith("G")) = ${list.indexWhere(_.startsWith("G"))}""")
    System.out.println(s"list.get(1) = ${list.get(1)}")
    System.out.println(s"list.reverse = ${list.reverse}")
    System.out.println(s"list.headOption = ${list.headOption}")
    System.out.println(s"list.lastOption = ${list.lastOption}")
    System.out.println(s"list.tail = ${list.tail}")
    System.out.println(s"list.pop = ${list.pop()}")
    System.out.println(s"list.lastOption = ${list.lastOption}")
    System.out.println(s"list = $list, list.size = ${list.size}")
    System.out.println( s"""list.prepend("Grapefruit") = ${list.prepend("Grapefruit")}, list.size = ${list.size}""")
    System.out.println(s"list = $list, list.size = ${list.size}")
    System.out.println(s"list.unshift = ${list.unshift()}")
    System.out.println(s"list = $list, list.size = ${list.size}")
  }

  case class QNode[T](var value: T, var previous: Option[QNode[T]] = None, var next: Option[QNode[T]] = None)

}
