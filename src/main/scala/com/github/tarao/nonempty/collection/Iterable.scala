package com.github.tarao.nonempty.collection

import scala.collection.StringOps
import scala.collection.immutable
import scala.collection.immutable.WrappedString
import scala.language.higherKinds
import scala.language.implicitConversions

trait FromIterable extends FromIterableLowPriority {
  protected def safeApply[A, C <: Iterable[A]](it: C): Option[NonEmpty[A, C]]

  /** Convert a `Map[A, B]` to `Option[NonEmpty[(A, B), Map[A, B]]]`.
    *
    * Note: There is no way to directly convert a collection into a
    *       `NonEmpty[_, _]`.
    *
    * Example {{{
    *   val ne = NonEmpty.from(Map(1 -> "foo"))
    * }}}
    *
    * @return `Some` refined `Map` value if it is neither empty nor
    *          mutable, or otherwise `None`.
    */
  implicit def from[K, V, CC[X, Y] <: scala.collection.Map[X, Y]](m: CC[K, V]): Option[NonEmpty[(K, V), CC[K, V]]] =
    safeApply[(K, V), CC[K, V]](m)

  /** Convert a `BitSet` to `Option[NonEmpty[Int, BitSet]]`.
    *
    * Note: There is no way to directly convert a collection into a
    *       `NonEmpty[_, _]`.
    *
    * Example {{{
    *   val ne = NonEmpty.from(BitSet(1, 2, 3))
    * }}}
    *
    * @return `Some` refined `BitSet` value if it is neither empty nor
    *          mutable, or otherwise `None`.
    */
  implicit def from[C <: scala.collection.BitSet](s: C): Option[NonEmpty[Int, C]] =
    safeApply[Int, C](s)

  /** Convert a `String` to `Option[NonEmpty[Char, WrappedString]]`.
    *
    * Note: There is no way to directly convert a collection into a
    *       `NonEmpty[_, _]`.
    *
    * Example {{{
    *   val ne = NonEmpty.from("foo")
    * }}}
    *
    * @return `Some` refined string if it is not empty, or otherwise
    *         `None`.
    */
  implicit def from(s: String): Option[NonEmpty[Char, WrappedString]] =
    safeApply[Char, WrappedString](s)
}

trait FromIterableLowPriority {
  self: FromIterable =>

  /** Convert a collection to `Option[NonEmpty[A, C]]` where the shape
    * of `C` is `CC[_]`.
    *
    * Note: There is no way to directly convert a collection into a
    *       `NonEmpty[_, _]`.
    *
    * Example {{{
    *   val ne = NonEmpty.from(List(1, 2, 3))
    * }}}
    *
    * @return `Some` refined collection value if it is neither empty
    *          nor mutable, or otherwise `None`.
    */
  implicit def from[A, CC[X] <: Iterable[X]](it: CC[A]): Option[NonEmpty[A, CC[A]]] =
    safeApply[A, CC[A]](it)
}

trait ToIterable {
  /** Treat a `NonEmpty[A, C]` as a `C`. */
  @inline
  implicit def toIterable[A, C <: Iterable[A]](ne: NonEmpty[A, C])(implicit
    factory: scala.collection.Factory[A, C]
  ): C = ne.value

  /** Treat a `NonEmpty[Char, WrappedString]` as a `String`. */
  @inline
  implicit def toString(ne: NonEmpty[Char, WrappedString]): String =
    ne.value.unwrap

  /** Treat a `NonEmpty[Char, WrappedString]` as a `StringOps`. */
  @inline
  implicit def toStringOps(ne: NonEmpty[Char, WrappedString]): StringOps =
    ne.value.unwrap
}
