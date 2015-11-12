package com.github.ldaniels528.just4fun

import com.github.ldaniels528.just4fun.BTree.BTNode

/**
 * Represents an unbalanced Binary Tree
 * @tparam T the given template type
 * @author lawrence.daniels@gmail.com
 */
class BTree[T <: Ordered[T]]() {
  private var root: Option[BTNode[T]] = None

  /**
   * Adds a value to the tree in logarithmic time
   * @param value the given value
   * @return true, if a new value was added
   */
  def add(value: T) = root match {
    case Some(node) => addChildValue(node, value)
    case None =>
      root = Option(BTNode(value))
      true
  }

  /**
   * Retrieves all values in ascending order
   * @return the values in ascending order
   */
  def ascending: List[T] = root.map(sortAscending(_, list = Nil)) getOrElse Nil

  /**
   * Retrieves all values in descending order
   * @return the values in descending order
   */
  def descending: List[T] = root.map(sortDescending(_, list = Nil)) getOrElse Nil

  /**
   * Adds a distinct value to the tree
   * @param parent the given [[BTNode parent node]]
   * @param value the given value
   * @return true, if a new value was added
   */
  private def addChildValue(parent: BTNode[T], value: T): Boolean = {
    value match {
      case v if v == parent.value => false
      case v if v < parent.value =>
        parent.left match {
          case Some(left) => addChildValue(left, v)
          case None =>
            parent.left = Some(BTNode(v))
            true
        }
      case v =>
        parent.right match {
          case Some(right) => addChildValue(right, v)
          case None =>
            parent.right = Some(BTNode(v))
            true
        }
    }
  }

  private def sortAscending(node: BTNode[T], list: List[T]): List[T] = {
    node.left.map(sortAscending(_, list)).getOrElse(Nil) ::: node.value :: node.right.map(sortAscending(_, list)).getOrElse(Nil) ::: list
  }

  private def sortDescending(node: BTNode[T], list: List[T]): List[T] = {
    node.right.map(sortDescending(_, list)).getOrElse(Nil) ::: node.value :: node.left.map(sortDescending(_, list)).getOrElse(Nil) ::: list
  }

}

/**
 * Binary Tree Companion
 * @author lawrence.daniels@gmail.com
 */
object BTree {

  /**
   * Creates a new binary tree pre-populated with the given values
   * @param values the optional set of values
   * @tparam T the given [[Ordered ordered value type]]
   * @return a new [[BTree binary tree]]
   */
  def apply[T <: Ordered[T]](values: T*) = {
    val tree = new BTree[T]()
    values foreach tree.add
    tree
  }

  /**
   * Represents a Binary Tree Node
   * @param value the given value
   * @param left the left-side [[BTNode binary tree node]]
   * @param right the right-side [[BTNode binary tree node]]
   * @tparam T the given template type
   */
  case class BTNode[T](value: T, var left: Option[BTNode[T]] = None, var right: Option[BTNode[T]] = None)

}
