package com.github.tarao
package nonempty

import org.scalatest.{FunSpec, Matchers, OptionValues, Inside, Inspectors}

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
    val t1: Traversable[T] = ne
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
      ne1 shouldBe a [NonEmpty[_]]

      val ne2 = NonEmpty(1, 2, 3)
      ne2 shouldBe a [NonEmpty[_]]
    }

    it("cannot be constructed from an empty parameter list") {
      assertTypeError("val ne = NonEmpty()")
    }

    it("should behave like a Iterable[]") {
      it should behave like iterable (NonEmpty(1))
      it should behave like iterable (NonEmpty(1, 2, 3))
    }

    it("should be identical to its Iterable[] counterpart") {
      val t1: Iterable[Int] = Seq(1)
      val ne1: Option[NonEmpty[Int]] = t1
      it should behave like identical (ne1.get, t1)

      val t2: Iterable[Int] = Seq(1, 2, 3)
      val ne2: Option[NonEmpty[Int]] = t2
      it should behave like identical (ne2.get, t2)

      val t3: Iterable[Int] = ArrayBuffer(1)
      val ne3: Option[NonEmpty[Int]] = t3
      it should behave like identical (ne3.get, t3)

      val t4: Iterable[Int] = ArrayBuffer(1, 2, 3)
      val ne4: Option[NonEmpty[Int]] = t4
      it should behave like identical (ne4.get, t4)
    }

    it("should preserve the collection type as NonEmpty[]") {
      val ne1 = NonEmpty(1, 2, 3) ++ Seq(4)
      ne1 shouldBe a[NonEmpty[_]]
      ne1.toSeq shouldBe Seq(1, 2, 3, 4)

      val m2 = NonEmpty(1, 2, 3, 4, 5, 6).groupBy(_ % 2 == 0)
      m2(true) shouldBe a[NonEmpty[_]]
      m2(true).toSeq shouldBe Seq(2, 4, 6)
      m2(false) shouldBe a[NonEmpty[_]]
      m2(false).toSeq shouldBe Seq(1, 3, 5)

      val v3 = NonEmpty(1, 2, 3, 4, 5).grouped(2).toVector
      v3(0) shouldBe a[NonEmpty[_]]
      v3(0).toSeq shouldBe Seq(1, 2)
      v3(1) shouldBe a[NonEmpty[_]]
      v3(1).toSeq shouldBe Seq(3, 4)
      v3(2) shouldBe a[NonEmpty[_]]
      v3(2).toSeq shouldBe Seq(5)

      val ne4 = NonEmpty(1, 2, 3).map(_ * 2)
      ne4 shouldBe a[NonEmpty[_]]
      ne4.toSeq shouldBe Seq(2, 4, 6)

      val ne5 = NonEmpty(1, 2, 3, 4).scan(0)((a, b) => a + b)
      ne5 shouldBe a[NonEmpty[_]]
      ne5.toSeq shouldBe Seq(0, 1, 3, 6, 10)

      val ne6 = NonEmpty(1, 2, 3, 4).scanLeft("0")((a, b) => a + b)
      ne6 shouldBe a[NonEmpty[_]]
      ne6.toSeq shouldBe Seq("0", "01", "012", "0123", "01234")

      val ne7 = NonEmpty(1, 2, 3, 4).scanRight("5")((a, b) => a + b)
      ne7 shouldBe a[NonEmpty[_]]
      ne7.toSeq shouldBe Seq("12345", "2345", "345", "45", "5")

      val ne8 = NonEmpty((1, "foo"), (2, "bar"), (3, "baz")).unzip
      ne8._1 shouldBe a[NonEmpty[_]]
      ne8._1.toSeq shouldBe Seq(1, 2, 3)
      ne8._2 shouldBe a[NonEmpty[_]]
      ne8._2.toSeq shouldBe Seq("foo", "bar", "baz")

      val ne9 = NonEmpty((1, "foo", 100), (2, "bar", 200), (3, "baz", 300)).unzip3
      ne9._1 shouldBe a[NonEmpty[_]]
      ne9._1.toSeq shouldBe Seq(1, 2, 3)
      ne9._2 shouldBe a[NonEmpty[_]]
      ne9._2.toSeq shouldBe Seq("foo", "bar", "baz")
      ne9._3 shouldBe a[NonEmpty[_]]
      ne9._3.toSeq shouldBe Seq(100, 200, 300)

      val neA = NonEmpty(1, 2, 3).zipAll(Seq("foo", "bar", "baz"), 0, "")
      neA shouldBe a[NonEmpty[_]]
      neA.toSeq(0) shouldBe ((1, "foo"))
      neA.toSeq(1) shouldBe ((2, "bar"))
      neA.toSeq(2) shouldBe ((3, "baz"))

      val neB = NonEmpty(1, 2, 3).zipAll(Seq(), 0, "qux")
      neB shouldBe a[NonEmpty[_]]
      neB.toSeq(0) shouldBe ((1, "qux"))
      neB.toSeq(1) shouldBe ((2, "qux"))
      neB.toSeq(2) shouldBe ((3, "qux"))

      val neC = NonEmpty(1, 2, 3).zipWithIndex
      neC shouldBe a[NonEmpty[_]]
      neC.toSeq(0) shouldBe ((1, 0))
      neC.toSeq(1) shouldBe ((2, 1))
      neC.toSeq(2) shouldBe ((3, 2))
    }

    it("should map to desired type by using breakOut") {
      val m: Map[Int, Int] = NonEmpty(1, 2, 3).map(i => i -> i*2)(breakOut)
      m shouldBe Map(1 -> 2, 2 -> 4, 3 -> 6)
    }
  }

  describe("Backward compatibility") {
    it("can be constructed from a traersable") {
      val Some(ne) = NonEmpty.fromTraversable(Seq(1, 2, 3))
      ne shouldBe a[NonEmpty[_]]
      ne.toSeq shouldBe Seq(1, 2, 3)
    }
  }
}
