package com.github.tarao

import scala.collection.generic.{CanBuildFrom => CollCanBuildFrom}

package object nonempty {
  /** Provides a `CanBuildFrom` instance that builds a specific target
    * collection (`To`) irrespective of the original collection
    * (`NonEmpty[A]`).
    */
  def breakOut[A, B, To](implicit
    bf: CollCanBuildFrom[Nothing, B, To]
  ): CanBuildFrom[A, B, To] =
    CanBuildFrom.OtherCanBuildFrom(scala.collection.breakOut(bf))
}
