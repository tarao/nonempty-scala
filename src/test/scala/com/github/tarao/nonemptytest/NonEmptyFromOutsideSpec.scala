package com.github.tarao
package nonemptytest

import org.scalatest.{FunSpec, Matchers, OptionValues, Inside, Inspectors}
import nonempty.NonEmpty

class NonEmptyFromOutsideSpec extends FunSpec
    with Matchers with OptionValues with Inside with Inspectors {
  describe("A value of NonEmpty") {
    it("cannot be directly instantiated from outside") {
      assertTypeError("val ne = new NonEmpty[Int](Seq(1))")
      assertTypeError("val ne = new nonempty.collection.NonEmpty[Int, Seq[Int]](Seq(1))")
    }
  }
}
