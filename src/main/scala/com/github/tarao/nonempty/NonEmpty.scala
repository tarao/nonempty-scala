package com.github.tarao
package nonempty

import scala.language.implicitConversions

sealed trait NonEmpty[+T] {
  def traversable: Traversable[T]
  override def toString = traversable.toString
  override def hashCode = traversable.hashCode
  override def equals(other: Any) = traversable.equals(other)
}
object NonEmpty {
  implicit def fromTraversable[T](t: Traversable[T]): Option[NonEmpty[T]] =
    if (t.isEmpty) None
    else Some(new NonEmpty[T]{ val traversable = t })

  implicit def toTraversable[T](ne: NonEmpty[T]): Traversable[T] =
    ne.traversable

  def apply[T](head: T, elements: T*): NonEmpty[T] = new NonEmpty[T] {
    val traversable = head +: Seq(elements: _*)
  }
}
