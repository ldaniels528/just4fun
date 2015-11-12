package com.github.ldaniels528.just4fun

import org.scalatest.Matchers._
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FeatureSpec, GivenWhenThen}

/**
 * String Helper Test Suite
 * @author lawrence.daniels@gmail.com
 */
class StringHelperTest extends FeatureSpec with GivenWhenThen with MockitoSugar {

  info("As a StringHelper singleton")
  info("I want to be able to perform various string transformations")

  feature("Ability to reverse all of the characters in a phrase") {
    scenario("Reverse all of the characters in a phrase") {
      Given("a phrase")
      val phrase = "Hello World"

      When("the phrase is reversed")
      val opposite = StringHelper.reversePhrase(phrase)

      Then("the phase should be the inverted opposite")
      opposite shouldBe "dlroW olleH"
    }
  }

  feature("Ability to reverse all of the characters in a sentence except for punctuation") {
    scenario("Reverse all of the characters in a sentence") {
      Given("a sentence")
      val sentence = "Hello World!"

      When("the phrase is reversed")
      val opposite = StringHelper.reverseSentence(sentence)

      Then("the phase should be the inverted opposite")
      opposite shouldBe "dlroW olleH!"
    }
  }

  feature("Ability to reverse all of the characters in a paragraph except for punctuation") {
    scenario("Reverse all of the characters in a paragraph") {
      Given("a paragraph")
      val paragraph = "Just make it stop. Please, make it stop!"

      When("the phrase is reversed")
      val opposite = StringHelper.reverseParagraph(paragraph)

      Then("the phase should be the inverted opposite")
      opposite shouldBe "pots ti ekam tsuJ. pots ti ekam ,esaelP!"
    }
  }

}
