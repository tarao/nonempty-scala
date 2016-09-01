package com.github.tarao
package nonempty

import scala.annotation.implicitNotFound
import scala.collection.generic.{CanBuildFrom => CollCanBuildFrom}

/** A base trait for builder factories.
  *
  * @tparam A  the type of the underlying collection that requests
  *            a builder to be created.
  * @tparam B  the element type of the collection to be created.
  * @tparam To the type of the collection to be created.
  */
@implicitNotFound(msg = "Cannot construct a collection of type ${To} with elements of type ${B} based on a collection of type ${A}.")
sealed trait CanBuildFrom[-A, B, +To] {
  private[nonempty] def canBuildFrom: CollCanBuildFrom[Iterable[A], B, Any]
  private[nonempty] def apply(from: Any): To
}
object CanBuildFrom {
  private[nonempty] case class NonEmptyCanBuildFrom[A, B](
    private[nonempty] val canBuildFrom: CollCanBuildFrom[Iterable[A], B, Iterable[B]]
  ) extends CanBuildFrom[A, B, NonEmpty[B]] {
    private[nonempty] def apply(from: Any): NonEmpty[B] =
      new NonEmpty(from.asInstanceOf[Iterable[B]])
  }
  private[nonempty] case class OtherCanBuildFrom[A, B, To](
    private[nonempty] val canBuildFrom: CollCanBuildFrom[Iterable[A], B, To]
  ) extends CanBuildFrom[A, B, To] {
    private[nonempty] def apply(x: Any): To = x.asInstanceOf[To]
  }

  implicit def nonEmptyCanBuildFrom[A, B](implicit
    bf: CollCanBuildFrom[Iterable[A], B, Iterable[B]]
  ): NonEmptyCanBuildFrom[A, B] = NonEmptyCanBuildFrom(bf)
}
