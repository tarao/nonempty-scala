package com.github.tarao.nonempty

import org.scalatest.{FunSpec, Inside, Inspectors, Matchers, OptionValues}
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class NonEmptySpec extends FunSpec
    with Matchers with OptionValues with Inside with Inspectors {
  def typeEquals[T1, T2](a: T1, b: T2)(implicit ev: T1 =:= T2): Boolean = true

  describe("NonEmpty[_] type") {
    it("should be an alias of generalized non-empty collection type") {
      val nel1 = NonEmpty(1, 2, 3)
      val nel2: NonEmpty[Int] = nel1
      val nel3: collection.NonEmpty[Int, Iterable[Int]] = nel1
      typeEquals(nel1, nel2)
      typeEquals(nel2, nel3)
    }
  }

  describe("Instantiation") {
    it("should provide apply(elem, ...) interface") {
      val nel1 = NonEmpty(1, 2, 3)
      val nel2 = NonEmpty[Int](1, 2, 3)
      val nel3: NonEmpty[Int] = nel1
      val nel4: NonEmpty[Int] = nel2
      typeEquals(nel1, nel2) shouldBe true
      typeEquals(nel2, nel3) shouldBe true
      typeEquals(nel3, nel4) shouldBe true
    }
  }

  describe("A context of type Option[NonEmpty[_]]") {
    it("should accept Iterable[_]") {
      val list: Option[NonEmpty[Int]] = Vector(1, 2, 3)
    }
  }

  describe("fromIterable") {
    it("should be identical to its immutable Iterable[_] counterpart") {
      locally {
        val it1 = List(1)
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = List(1, 2, 3)
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[Int] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = LazyList(1)
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = LazyList(1, 2, 3)
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[Int] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.Queue(1)
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.Queue(1, 2, 3)
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[Int] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = Vector(1)
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = Vector(1, 2, 3)
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[Int] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = Set(1)
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = Set(1, 2, 3)
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[Int] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.HashSet(1)
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.HashSet(1, 2, 3)
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[Int] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.SortedSet(1)
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.SortedSet(1, 2, 3)
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[Int] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.ListSet(1)
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.ListSet(1, 2, 3)
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[Int] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.TreeSet(1)
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.TreeSet(1, 2, 3)
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[Int] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.BitSet(1)
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.BitSet(1, 2, 3)
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[Int] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = Map(1 -> "foo")
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = Map(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[(Int, String)] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.HashMap(1 -> "foo")
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.HashMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[(Int, String)] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.SortedMap(1 -> "foo")
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.SortedMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[(Int, String)] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.ListMap(1 -> "foo")
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.ListMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[(Int, String)] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.TreeMap(1 -> "foo")
        val Some(nel1) = NonEmpty.fromIterable(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.TreeMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel2) = NonEmpty.fromIterable(it2)
        nel2.value should be theSameInstanceAs it2

        val nel3: NonEmpty[(Int, String)] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }
    }

    it("should freeze the underlying mutable collection") {
      locally {
        val it = mutable.Stack(1, 2, 3)
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.Queue(1, 2, 3)
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.PriorityQueue(1, 2, 3)
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.ArraySeq(1, 2, 3)
        val Some(nel) = NonEmpty.fromIterable(it)
        it.update(1, 20)
        it.sum shouldBe 24
        nel.sum shouldBe 6
      }

      locally {
        val it = mutable.ArrayBuffer(1, 2, 3)
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.ListBuffer(1, 2, 3)
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.HashSet(1, 2, 3)
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.LinkedHashSet(1, 2, 3)
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.TreeSet(1, 2, 3)
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.BitSet(1, 2, 3)
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.HashMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.LinkedHashMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }

      locally {
        val it = mutable.TreeMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel) = NonEmpty.fromIterable(it)
        it.clear()
        it.size shouldBe 0
        nel.size shouldBe 3
      }
    }
  }
}
