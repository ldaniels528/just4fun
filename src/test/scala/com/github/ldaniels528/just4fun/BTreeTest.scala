package com.github.ldaniels528.just4fun

import org.scalatest.Matchers._
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FeatureSpec, GivenWhenThen}

import scala.language.implicitConversions

/**
 * Binary Tree Test Suite
 * @author lawrence.daniels@gmail.com
 */
class BTreeTest extends FeatureSpec with GivenWhenThen with MockitoSugar {

  info("As a BTree instance")
  info("I want to be able to maintain a collection of values in a pre-sorted order")

  feature("Ability to retrieve elements in ascending order") {
    scenario("Add a set of values to a tree and retrieve them in ascending order") {
      Given("a binary tree")
      val tree = BTree[OrderedValue](10, 5, 8, 7, 1, 6, 2, 13, 11, 9, 1)

      When("an ascending sort is retrieved")
      val values = tree.ascending

      Then("the distinct set of values should appear in ascending order")
      info(s"values = $values")
      values shouldBe List[OrderedValue](1, 2, 5, 6, 7, 8, 9, 10, 11, 13)
    }
  }

  feature("Ability to retrieve elements in descending order") {
    scenario("Add a set of values to a tree and retrieve them in descending order") {
      Given("a binary tree")
      val tree = BTree[OrderedValue](10, 5, 8, 7, 1, 6, 2, 13, 11, 9, 1)

      When("a descending sort is retrieved")
      val values = tree.descending

      Then("the distinct set of values should appear in descending order")
      info(s"values = $values")
      values shouldBe List[OrderedValue](13, 11, 10, 9, 8, 7, 6, 5, 2, 1)
    }
  }

}

/**
 * Represents an order value implementation
 * @param value the underlying value
 * @author lawrence.daniels@gmail.com
 */
case class OrderedValue(value: Int) extends Ordered[OrderedValue] {
  override def compare(that: OrderedValue) = this.value - that.value

  override def toString = value.toString
}

/**
 * Ordered Value Companion
 * @author lawrence.daniels@gmail.com
 */
object OrderedValue {

  implicit def int2OrderedValue(value: Int): OrderedValue = OrderedValue(value)

}