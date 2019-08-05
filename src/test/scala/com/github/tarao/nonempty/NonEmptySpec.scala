package com.github.tarao
package nonempty

import eu.timepit.refined
import eu.timepit.refined.refineV
import eu.timepit.refined.api.{Refined, RefType}
import org.scalatest.{FunSpec, Inside, Inspectors, Matchers, OptionValues}
import scala.language.higherKinds

class NonEmptySpec extends FunSpec
    with Matchers with OptionValues with Inside with Inspectors {
  import scala.collection.mutable.ArrayBuffer

  object AsParameterOf {
    def optionNonEmpty[T](ne: Option[NonEmpty[T]]): Unit = {}
    def nonEmpty[T](ne: NonEmpty[T]): Unit = {}
    def iterable[T](t: Iterable[T]): Unit = {}
  }

  def iterable[T](ne: NonEmpty[T]): Unit = {
    // as a variable
    val it1: Iterable[T] = ne

    // as a parameter
    AsParameterOf.iterable(ne)

    // methods
    ne.map(_.toString)
    ne.mkString
  }

  def identical[T](ne: NonEmpty[T], t: Iterable[T]): Unit = {
    ne should equal(NonEmpty.fromIterable(t).get)
    val t1: Iterable[T] = ne
    t1 should equal(t)
    ne.toString should equal (t.toString)
    ne.hashCode should equal (t.hashCode)
  }

  describe("A context of type Option[NonEmpty[]]") {
    it("should accept a Iterable[]") {
      // as a varibable
      val ne1: Option[NonEmpty[Int]] = Seq(1, 2, 3)
      val ne2: Option[NonEmpty[Int]] = Seq.empty[Int]
      val ne3: Option[NonEmpty[Int]] = ArrayBuffer(1, 2, 3)
      val ne4: Option[NonEmpty[Int]] = ArrayBuffer.empty[Int]

      // as a parameter
      AsParameterOf.optionNonEmpty(Seq(1, 2, 3))
      AsParameterOf.optionNonEmpty(Seq.empty[Int])
      AsParameterOf.optionNonEmpty(ArrayBuffer(1, 2, 3))
      AsParameterOf.optionNonEmpty(ArrayBuffer.empty[Int])
    }
  }

  describe("A context of type Option[NonEmpty[]]") {
    it("should not accept a Iterable[]") {
      // as a varibable
      assertTypeError("val ne1: NonEmpty[Int] = Seq(1, 2, 3)")
      assertTypeError("val ne2: NonEmpty[Int] = Seq.empty[Int]")
      assertTypeError("val ne3: NonEmpty[Int] = ArrayBuffer(1, 2, 3)")
      assertTypeError("val ne4: NonEmpty[Int] = ArrayBuffer.empty[Int]")

      // as a parameter
      assertTypeError("nonEmpty(Seq(1, 2, 3))")
      assertTypeError("nonEmpty(Seq.empty[Int])")
      assertTypeError("nonEmpty(ArrayBuffer(1, 2, 3))")
      assertTypeError("nonEmpty(ArrayBuffer.empty[Int])")
    }
  }

  describe("A value of NonEmpty") {
    it("can be constructed from a non-empty parameter list") {
      val ne1 = NonEmpty(1)
      ne1.isInstanceOf[NonEmpty[_]] shouldBe true

      val ne2 = NonEmpty(1, 2, 3)
      ne2.isInstanceOf[NonEmpty[_]] shouldBe true
    }

    it("cannot be constructed from an empty parameter list") {
      assertTypeError("val ne = NonEmpty()")
    }

    it("should behave like a Iterable[]") {
      it should behave like iterable (NonEmpty(1))
      it should behave like iterable (NonEmpty(1, 2, 3))
    }

    it("should be identical to its immutable Iterable[] counterpart") {
      locally {
        val t1: Iterable[Int] = List(1)
        val ne1: Option[NonEmpty[Int]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[Int] = List(1, 2, 3)
        val ne2: Option[NonEmpty[Int]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[Int] = scala.collection.immutable.LazyList(1)
        val ne1: Option[NonEmpty[Int]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[Int] = scala.collection.immutable.LazyList(1, 2, 3)
        val ne2: Option[NonEmpty[Int]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[Int] = scala.collection.immutable.Queue(1)
        val ne1: Option[NonEmpty[Int]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[Int] = scala.collection.immutable.Queue(1, 2, 3)
        val ne2: Option[NonEmpty[Int]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[Int] = Vector(1)
        val ne1: Option[NonEmpty[Int]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[Int] = Vector(1, 2, 3)
        val ne2: Option[NonEmpty[Int]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[Int] = 1 to 1
        val ne1: Option[NonEmpty[Int]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[Int] = 1 to 3
        val ne2: Option[NonEmpty[Int]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[Int] = Set(1)
        val ne1: Option[NonEmpty[Int]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[Int] = Set(1, 2, 3)
        val ne2: Option[NonEmpty[Int]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[Int] = scala.collection.immutable.HashSet(1)
        val ne1: Option[NonEmpty[Int]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[Int] = scala.collection.immutable.HashSet(1, 2, 3)
        val ne2: Option[NonEmpty[Int]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[Int] = scala.collection.immutable.SortedSet(1)
        val ne1: Option[NonEmpty[Int]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[Int] = scala.collection.immutable.SortedSet(1, 2, 3)
        val ne2: Option[NonEmpty[Int]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[Int] = scala.collection.immutable.BitSet(1)
        val ne1: Option[NonEmpty[Int]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[Int] = scala.collection.immutable.BitSet(1, 2, 3)
        val ne2: Option[NonEmpty[Int]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[Int] = scala.collection.immutable.ListSet(1)
        val ne1: Option[NonEmpty[Int]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[Int] = scala.collection.immutable.ListSet(1, 2, 3)
        val ne2: Option[NonEmpty[Int]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[(Int, String)] = Map(1 -> "foo")
        val ne1: Option[NonEmpty[(Int, String)]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[(Int, String)] = Map(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val ne2: Option[NonEmpty[(Int, String)]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[(Int, String)] = scala.collection.immutable.HashMap(1 -> "foo")
        val ne1: Option[NonEmpty[(Int, String)]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[(Int, String)] = scala.collection.immutable.HashMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val ne2: Option[NonEmpty[(Int, String)]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[(Int, String)] = scala.collection.immutable.SortedMap(1 -> "foo")
        val ne1: Option[NonEmpty[(Int, String)]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[(Int, String)] = scala.collection.immutable.SortedMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val ne2: Option[NonEmpty[(Int, String)]] = t2
        it should behave like identical (ne2.get, t2)
      }

      locally {
        val t1: Iterable[(Int, String)] = scala.collection.immutable.ListMap(1 -> "foo")
        val ne1: Option[NonEmpty[(Int, String)]] = t1
        it should behave like identical (ne1.get, t1)

        val t2: Iterable[(Int, String)] = scala.collection.immutable.ListMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val ne2: Option[NonEmpty[(Int, String)]] = t2
        it should behave like identical (ne2.get, t2)
      }
    }

    it("should freeze a mutable collection") {
      locally {
        val t = scala.collection.mutable.Stack(1, 2, 3)
        val ne: Option[NonEmpty[Int]] = t
        t.clear()
        ne.get.size shouldBe 3
      }

      locally {
        val t = scala.collection.mutable.PriorityQueue(1, 2, 3)
        val ne: Option[NonEmpty[Int]] = t
        t.clear()
        ne.get.size shouldBe 3
      }

      locally {
        val t = scala.collection.mutable.ArraySeq(1, 2, 3)
        val ne: Option[NonEmpty[Int]] = t
        t.drop(3)
        ne.get.size shouldBe 3
      }

      locally {
        val t = scala.collection.mutable.ArrayBuffer(1, 2, 3)
        val ne: Option[NonEmpty[Int]] = t
        t.clear()
        ne.get.size shouldBe 3
      }

      locally {
        val t = scala.collection.mutable.ListBuffer(1, 2, 3)
        val ne: Option[NonEmpty[Int]] = t
        t.clear()
        ne.get.size shouldBe 3
      }

      locally {
        val t = scala.collection.mutable.HashSet(1, 2, 3)
        val ne: Option[NonEmpty[Int]] = t
        t.clear()
        ne.get.size shouldBe 3
      }

      locally {
        val t = scala.collection.mutable.BitSet(1, 2, 3)
        val ne: Option[NonEmpty[Int]] = t
        t.clear()
        ne.get.size shouldBe 3
      }

      locally {
        val t = scala.collection.mutable.LinkedHashSet(1, 2, 3)
        val ne: Option[NonEmpty[Int]] = t
        t.clear()
        ne.get.size shouldBe 3
      }

      locally {
        val t = scala.collection.mutable.HashMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val ne: Option[NonEmpty[(Int, String)]] = t
        t.clear()
        ne.get.size shouldBe 3
      }

      locally {
        val t = scala.collection.mutable.LinkedHashMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val ne: Option[NonEmpty[(Int, String)]] = t
        t.clear()
        ne.get.size shouldBe 3
      }
    }

    it("should preserve the collection type as NonEmpty[]") {
      val ne1 = NonEmpty(1, 2, 3) ++ Seq(4)
      ne1.isInstanceOf[NonEmpty[_]] shouldBe true
      ne1.toSeq shouldBe Seq(1, 2, 3, 4)

      val m2 = NonEmpty(1, 2, 3, 4, 5, 6).groupBy(_ % 2 == 0)
      m2(true).isInstanceOf[NonEmpty[_]] shouldBe true
      m2(true).toSeq shouldBe Seq(2, 4, 6)
      m2(false).isInstanceOf[NonEmpty[_]] shouldBe true
      m2(false).toSeq shouldBe Seq(1, 3, 5)

      val v3 = NonEmpty(1, 2, 3, 4, 5).grouped(2).toVector
      v3(0).isInstanceOf[NonEmpty[_]] shouldBe true
      v3(0).toSeq shouldBe Seq(1, 2)
      v3(1).isInstanceOf[NonEmpty[_]] shouldBe true
      v3(1).toSeq shouldBe Seq(3, 4)
      v3(2).isInstanceOf[NonEmpty[_]] shouldBe true
      v3(2).toSeq shouldBe Seq(5)

      val ne4 = NonEmpty(1, 2, 3).map(_ * 2)
      ne4.isInstanceOf[NonEmpty[_]] shouldBe true
      ne4.toSeq shouldBe Seq(2, 4, 6)

      val ne5 = NonEmpty(1, 2, 3, 4).scan(0)((a, b) => a + b)
      ne5.isInstanceOf[NonEmpty[_]] shouldBe true
      ne5.toSeq shouldBe Seq(0, 1, 3, 6, 10)

      val ne6 = NonEmpty(1, 2, 3, 4).scanLeft("0")((a, b) => a + b)
      ne6.isInstanceOf[NonEmpty[_]] shouldBe true
      ne6.toSeq shouldBe Seq("0", "01", "012", "0123", "01234")

      val ne7 = NonEmpty(1, 2, 3, 4).scanRight("5")((a, b) => a.toString + b)
      ne7.isInstanceOf[NonEmpty[_]] shouldBe true
      ne7.toSeq shouldBe Seq("12345", "2345", "345", "45", "5")

      val ne8 = NonEmpty((1, "foo"), (2, "bar"), (3, "baz")).unzip
      ne8._1.isInstanceOf[NonEmpty[_]] shouldBe true
      ne8._1.toSeq shouldBe Seq(1, 2, 3)
      ne8._2.isInstanceOf[NonEmpty[_]] shouldBe true
      ne8._2.toSeq shouldBe Seq("foo", "bar", "baz")

      val ne9 = NonEmpty((1, "foo", 100), (2, "bar", 200), (3, "baz", 300)).unzip3
      ne9._1.isInstanceOf[NonEmpty[_]] shouldBe true
      ne9._1.toSeq shouldBe Seq(1, 2, 3)
      ne9._2.isInstanceOf[NonEmpty[_]] shouldBe true
      ne9._2.toSeq shouldBe Seq("foo", "bar", "baz")
      ne9._3.isInstanceOf[NonEmpty[_]] shouldBe true
      ne9._3.toSeq shouldBe Seq(100, 200, 300)

      val neA = NonEmpty(1, 2, 3).zipAll(Seq("foo", "bar", "baz"), 0, "")
      neA.isInstanceOf[NonEmpty[_]] shouldBe true
      neA.toSeq(0) shouldBe ((1, "foo"))
      neA.toSeq(1) shouldBe ((2, "bar"))
      neA.toSeq(2) shouldBe ((3, "baz"))

      val neB = NonEmpty(1, 2, 3).zipAll(Seq(), 0, "qux")
      neB.isInstanceOf[NonEmpty[_]] shouldBe true
      neB.toSeq(0) shouldBe ((1, "qux"))
      neB.toSeq(1) shouldBe ((2, "qux"))
      neB.toSeq(2) shouldBe ((3, "qux"))

      val neC = NonEmpty(1, 2, 3).zipWithIndex
      neC.isInstanceOf[NonEmpty[_]] shouldBe true
      neC.toSeq(0) shouldBe ((1, 0))
      neC.toSeq(1) shouldBe ((2, 1))
      neC.toSeq(2) shouldBe ((3, 2))
    }

    it("should return Iterable if the number of elements may reduce") {
      val l1 = NonEmpty(Seq(1, 2), Seq(3, 4)).flatten
      l1 shouldBe a[Iterable[_]]

      val l2 = NonEmpty(Seq(), Seq()).flatten
      l2 shouldBe a[Iterable[_]]

      val l3 = NonEmpty(1, 2, 3).drop(1)
      l3 shouldBe a[Iterable[_]]

      val l4 = NonEmpty(1, 2, 3).drop(5)
      l4 shouldBe a[Iterable[_]]

      val l5 = NonEmpty(1, 2, 3).take(1)
      l5 shouldBe a[Iterable[_]]

      val l6 = NonEmpty(1, 2, 3).take(0)
      l6 shouldBe a[Iterable[_]]
    }

    it("should map to desired type by using `to`") {
      val m: Map[Int, Int] = NonEmpty(1, 2, 3).map(i => i -> i*2).to(Map)
      m shouldBe Map(1 -> 2, 2 -> 4, 3 -> 6)
    }
  }

  describe("Compatibility with refined") {
    def compatibleWithRefined[A, L[X] <: Iterable[X], F[_, _]](
      ne: F[L[A], refined.collection.NonEmpty]
    )(implicit rt: RefType[F]): Unit = { /* ok */ }

    it("should be compatible with refined") {
      val ne = NonEmpty(1)
      it should behave like compatibleWithRefined(ne)

      val Right(nel) = refineV[refined.collection.NonEmpty](Seq(1, 2, 3))
      val ne1 = NonEmpty.fromRefined(nel)
      val ne2: NonEmpty[Int] = nel

      val nel1: refined.api.Refined[Iterable[Int], refined.collection.NonEmpty] = ne
    }
  }
}
