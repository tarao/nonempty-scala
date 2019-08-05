package com.github.tarao.nonempty.collection

import scala.collection.StringOps
import scala.collection.immutable.WrappedString
import scala.language.higherKinds
import scala.language.implicitConversions

trait FromIterable extends FromIterableLowPriority {
  protected def unsafeApply[A, C <: Iterable[A]](it: C) : NonEmpty[A, C]
  protected def unsafeImmutableApply[A, C <: Iterable[A]](it: C)(implicit
    factory: scala.collection.Factory[A, C]
  ) : NonEmpty[A, C]

  /** Convert a `Map[A, B]` to `Option[NonEmpty[(A, B), Map[A, B]]]`.
    *
    * Note: There is no way to directly convert a collection into a
    *       `NonEmpty[_, _]`.
    *
    * Note: If you pass a mutable collection as `it`, a copy is made
    *       to avoid dropping the elements.
    *
    * Example {{{
    *   val ne = NonEmpty.from(Map(1 -> "foo"))
    * }}}
    */
  implicit def from[K, V, CC[X, Y] <: scala.collection.Map[X, Y]](m: CC[K, V])(implicit
    factory: scala.collection.Factory[(K, V), CC[K, V]]
  ): Option[NonEmpty[(K, V), CC[K, V]]] =
    if (m.isEmpty) None
    else Some(unsafeImmutableApply[(K, V), CC[K, V]](m))

  /** Convert a `BitSet` to `Option[NonEmpty[Int, BitSet]]`.
    *
    * Note: There is no way to directly convert a collection into a
    *       `NonEmpty[_, _]`.
    *
    * Note: If you pass a mutable collection as `it`, a copy is made
    *       to avoid dropping the elements.
    *
    * Example {{{
    *   val ne = NonEmpty.from(BitSet(1, 2, 3))
    * }}}
    */
  implicit def from[C <: scala.collection.BitSet](s: C)(implicit
    factory: scala.collection.Factory[Int, C]
  ): Option[NonEmpty[Int, C]] =
    if (s.isEmpty) None
    else Some(unsafeImmutableApply[Int, C](s))

  /** Convert a `String` to `Option[NonEmpty[Char, WrappedString]]`.
    *
    * Note: There is no way to directly convert a collection into a
    *       `NonEmpty[_, _]`.
    *
    * Note: If you pass a mutable collection as `it`, a copy is made
    *       to avoid dropping the elements.
    *
    * Example {{{
    *   val ne = NonEmpty.from("foo")
    * }}}
    */
  implicit def from(s: String)(implicit
    factory: scala.collection.Factory[Char, WrappedString]
  ): Option[NonEmpty[Char, WrappedString]] =
    if (s.isEmpty) None
    else Some(unsafeApply[Char, WrappedString](s))
}

trait FromIterableLowPriority {
  self: FromIterable =>

  /** Convert a collection to `Option[NonEmpty[A, C]]` where the shape
    * of `C` is `CC[_]`.
    *
    * Note: There is no way to directly convert a collection into a
    *       `NonEmpty[_, _]`.
    *
    * Note: If you pass a mutable collection as `it`, a copy is made
    *       to avoid dropping the elements.
    *
    * Example {{{
    *   val ne = NonEmpty.from(List(1, 2, 3))
    * }}}
    */
  implicit def from[A, CC[X] <: Iterable[X]](it: CC[A])(implicit
    factory: scala.collection.Factory[A, CC[A]]
  ): Option[NonEmpty[A, CC[A]]] =
    if (it.isEmpty) None
    else Some(unsafeImmutableApply[A, CC[A]](it))
}

trait ToIterable {
  /** Treat a `NonEmpty[A, C]` as a `C`.
    *
    * Note: If `C` is a mutable collection type, it makes a copy of
    *       the underlying collection instance to avoid dropping the
    *       elements.
    */
  implicit def toIterable[A, C <: Iterable[A]](ne: NonEmpty[A, C])(implicit
    factory: scala.collection.Factory[A, C]
  ): C = ne.value match {
    case _: scala.collection.mutable.Iterable[_] =>
      factory.newBuilder.addAll(ne.value).result
    case _ => ne.value
  }

  /** Treat a `NonEmpty[Char, WrappedString]` as a `String`. */
  implicit def toString(ne: NonEmpty[Char, WrappedString]): String =
    ne.value.unwrap

  /** Treat a `NonEmpty[Char, WrappedString]` as a `StringOps`. */
  implicit def toStringOps(ne: NonEmpty[Char, WrappedString]): StringOps =
    ne.value.unwrap
}
