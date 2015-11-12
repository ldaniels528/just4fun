package com.github.ldaniels528.just4fun

import scala.language.postfixOps

/**
 * A Simple Hash Table implementation
 * @tparam K the given template type of the key
 * @tparam V the given template type of the value
 * @author lawrence.daniels@gmail.com
 */
class HashTable[K, V]() {
  private val buckets: Array[List[(K, V)]] = (1 to 100) map (_ => Nil) toArray

  def get(key: K): Option[V] = {
    val index = key.hashCode() % buckets.length
    buckets(index) find { case (k, v) => k == key } map (_._2)
  }

  def put(key: K, value: V) = {
    val index = Math.abs(key.hashCode()) % buckets.length
    val bucket = buckets(index)
    bucket.indexWhere { case (k, v) => k == key } match {
      case -1 => buckets(index) = (key, value) :: buckets(index)
      case keyIndex => buckets(index) = (key, value) :: bucket.slice(0, keyIndex) ::: bucket.slice(keyIndex + 1, buckets.length)
    }
  }

  def dump(): Unit = {
    buckets.indices zip buckets foreach { case (index, bucket) =>
      bucket foreach { case (key, value) =>
        System.err.println(s"[$index] '$key' -> '$value'")
      }
    }
  }

}

/**
 * Hash Table Companion
 * @author lawrence.daniels@gmail.com
 */
object HashTable {

  def main(args: Array[String]) = {
    val ht = new HashTable[String, String]()
    ht.put("Hello", "World")
    ht.put("Hello", "World x 2")
    ht.put("Apple", "Fruit")
    ht.put("Orange", "Fruit")
    ht.put("Tomato", "Fruit")
    ht.dump()

    ht.get("Hello") foreach System.out.println
    ht.get("Apple") foreach System.out.println
  }

}