package com.github.tarao
package nonempty

import scala.annotation.implicitNotFound
import scala.collection.generic.{CanBuildFrom => CollCanBuildFrom}
import scala.collection.mutable.Builder

/** A base trait for builder factories.
  *
  * @tparam A  the type of the underlying collection that requests
  *            a builder to be created.
  * @tparam B  the element type of the collection to be created.
  * @tparam To the type of the collection to be created.
  */
@implicitNotFound(msg = "Cannot construct a collection of type ${To} with elements of type ${B} based on a collection of type ${A}.")
sealed trait CanBuildFrom[-A, B, +To] extends Any {
  private[nonempty] def canBuildFrom: CollCanBuildFrom[Iterable[A], B, To]
}
object CanBuildFrom {
  private[nonempty] case class NonEmptyCanBuildFrom[A, B](
    val bf: CollCanBuildFrom[Iterable[A], B, Iterable[B]]
  ) extends AnyVal with CanBuildFrom[A, B, NonEmpty[B]] {
    private type From = Iterable[A]
    private type To = NonEmpty[B]
    private[nonempty] def canBuildFrom: CollCanBuildFrom[From, B, To] =
      new CollCanBuildFrom[From, B, To] {
        def apply(from: From): Builder[B, To] =
          bf.apply(from).mapResult(new NonEmpty(_))
        def apply(): Builder[B, To] =
          bf.apply().mapResult(new NonEmpty(_))
      }
  }
  private[nonempty] case class OtherCanBuildFrom[A, B, To](
    val bf: CollCanBuildFrom[Iterable[A], B, To]
  ) extends AnyVal with CanBuildFrom[A, B, To] {
    private type From = Iterable[A]
    private[nonempty] def canBuildFrom: CollCanBuildFrom[From, B, To] = bf
  }

  implicit def nonEmptyCanBuildFrom[A, B](implicit
    bf: CollCanBuildFrom[Iterable[A], B, Iterable[B]]
  ): NonEmptyCanBuildFrom[A, B] = NonEmptyCanBuildFrom(bf)
}
