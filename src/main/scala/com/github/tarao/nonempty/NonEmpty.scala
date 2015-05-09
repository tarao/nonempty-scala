package com.github.tarao
package nonempty

import scala.language.implicitConversions

/** A value class for non-empty traversable collections.
  *
  * It is intended to be used as a parameter type to restrict a
  * collection value to be non-empty.  This restriction is guaranteed
  * statically, i.e., it is not possible to pass an empty collection
  * as a parameter of type NonEmpty[].
  *
  * The static safety is guaranteed by two reasons:
  * 1. there is no way to create an instance of NonEmpty[] other than
  *    by factory methods provided by singleton object NonEmpty
  * 2. there is only two factory methods:
  *    - an implicit conversion from Traversable[T] to
  *      Option[NonEmpty[T]] where an empty value will be None
  *    - an apply which takes at least one argument
  *
  * A value of type NonEmpty[T] can be used as a Traversable[T] by an
  * implicit conversion from NonEmpty[T] to Traversable[T].
  */
class NonEmpty[+T] private (val traversable: Traversable[T]) extends AnyVal {
  override def toString = traversable.toString
}
object NonEmpty {
  /** Convert a Traversable[T] to Option[NonEmpty[T]].
    * There is no way to directly convert a Traversable[T] into a NonEmpty[T].
    */
  implicit def fromTraversable[T](t: Traversable[T]): Option[NonEmpty[T]] =
    if (t.isEmpty) None
    else Some(new NonEmpty[T](t))

  /** Automatically treat a NonEmpty[T] as a Traversable[T]. */
  implicit def toTraversable[T](ne: NonEmpty[T]): Traversable[T] =
    ne.traversable

  /** Directly create a NonEmpty[T] by passing no less than one parameter. */
  def apply[T](head: T, elements: T*): NonEmpty[T] =
    new NonEmpty[T](head +: Seq(elements: _*))
}
