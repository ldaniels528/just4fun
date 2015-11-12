package com.github.ldaniels528.just4fun

import org.scalatest.Matchers._
import org.scalatest.mock.MockitoSugar
import org.scalatest.{GivenWhenThen, FeatureSpec}

/**
 * Mathematics Test Suite
 * @author lawrence.daniels@gmail.com
 */
class MathematicsTest extends FeatureSpec with GivenWhenThen with MockitoSugar {

  info("As a Mathematics singleton")
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

}
