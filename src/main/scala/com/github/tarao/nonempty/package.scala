package com.github.tarao

import scala.collection.generic.{CanBuildFrom => CollCanBuildFrom}

package object nonempty {
  /** Provides a CanBuildFrom instance that builds a specific target
    * collection (`To') irrespective of the original collection
    * (`From').
    */
  def breakOut[From, T, To](implicit
    bf: CollCanBuildFrom[Nothing, T, To]
  ): CanBuildFrom[From, T, To] =
    CanBuildFrom.OtherCanBuildFrom(scala.collection.breakOut)
}
