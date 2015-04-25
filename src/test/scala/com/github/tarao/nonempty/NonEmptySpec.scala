package com.github.tarao
package nonempty

class NonEmptySpec extends UnitSpec {
  import scala.collection.mutable.ArrayBuffer

  object AsParameterOf {
    def optionNonEmpty[T](ne: Option[NonEmpty[T]]) {}
    def nonEmpty[T](ne: NonEmpty[T]) {}
    def traversable[T](t: Traversable[T]) {}
  }

  def traversable[T](ne: NonEmpty[T]) {
    // as a variable
    val t1: Traversable[T] = ne

    // as a parameter
    AsParameterOf.traversable(ne)

    // methods
    ne.map(_.toString)
    ne.mkString
  }

  def identical[T](ne: NonEmpty[T], t: Traversable[T]) {
    ne should equal (t)
    ne.toString should equal (t.toString)
    ne.hashCode should equal (t.hashCode)
  }

  describe("A context of type Option[NonEmpty[]]") {
    it("should accept a Traversable[]") {
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
    it("should not accept a Traversable[]") {
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

    it("cannot be directly instantiated from outside") {
      assertTypeError("val ne = new NonEmpty[Int] {}")
    }

    it("should behave like a Traversable[]") {
      it should behave like traversable (NonEmpty(1))
      it should behave like traversable (NonEmpty(1, 2, 3))
    }

    it("should be identical to its Traversable[] counterpart") {
      val t1: Traversable[Int] = Seq(1)
      val ne1: Option[NonEmpty[Int]] = t1
      it should behave like identical (ne1.get, t1)

      val t2: Traversable[Int] = Seq(1, 2, 3)
      val ne2: Option[NonEmpty[Int]] = t2
      it should behave like identical (ne2.get, t2)

      val t3: Traversable[Int] = ArrayBuffer(1)
      val ne3: Option[NonEmpty[Int]] = t3
      it should behave like identical (ne3.get, t3)

      val t4: Traversable[Int] = ArrayBuffer(1, 2, 3)
      val ne4: Option[NonEmpty[Int]] = t4
      it should behave like identical (ne4.get, t4)
    }
  }
}
