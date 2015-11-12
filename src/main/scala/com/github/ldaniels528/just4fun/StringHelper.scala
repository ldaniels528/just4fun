package com.github.ldaniels528.just4fun

/**
 * Reverse Helper
 * @author lawrence.daniels@gmail.com
 */
object StringHelper {

  def main(args: Array[String]) = {
    val text1 = "12345"
    System.out.println(s"text1 = '$text1', value = ${asciiToInt(text1)}")

    val text2 = "72388"
    System.out.println(s"text2 = '$text2', value = ${minimumConsecutiveSlice(text2, 3)}")
  }

  def asciiToInt(number: String) = {
    number.reverse.toCharArray.foldLeft((0, 1)) { case ((result, factor), ch) =>
      result + (ch - '0') * factor -> factor * 10
    }._1
  }

  def minimumConsecutiveSlice(number: String, digits: Int) = {
    (0 to number.length - digits).foldLeft(Int.MaxValue) { (minimum, index) =>
      Math.min(number.substring(index, index + digits).toInt, minimum)
    }
  }

  def reversePhrase(phrase: String) = {
    val chars = phrase.toCharArray
    val limit = chars.length - 1
    for (n <- 0 to limit / 2) {
      val (a, b) = (chars(n), chars(limit - n))
      chars(n) = b
      chars(limit - n) = a
    }
    new String(chars)
  }

  def reverseSentence(sentence: String) = {
    ".!?".toCharArray.foldLeft(reversePhrase(sentence)) { (rv, pc) =>
      if (rv.headOption.contains(pc)) rv.substring(1) + pc else rv
    }
  }

  def reverseParagraph(paragraph: String) = {
    val chars = ".!?".toCharArray
    var remaining = paragraph
    var list: List[String] = Nil
    while (chars.map(ch => remaining.indexOf(ch)).filterNot(_ == -1).sortWith(_ < _).headOption.exists { index =>
      list = remaining.substring(0, index + 1).trim :: list
      remaining = remaining.substring(index + 1).trim
      true
    }) {}

    (remaining :: list).reverse.map(s => if (s.nonEmpty) reverseSentence(s) else s).mkString(" ").trim
  }

}
