package com.github.tarao.nonempty

import org.scalatest.{FunSpec, Inside, Inspectors, Matchers, OptionValues}

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
}
