package com.github.tarao.nonempty
package collection

import eu.timepit.refined
import eu.timepit.refined.api.RefType
import scala.collection.immutable.WrappedString
import scala.language.higherKinds
import scala.language.implicitConversions

trait FromRefined extends FromRefinedLowPriority {
  protected def unsafeApply[A, C <: Iterable[A]](it: C) : NonEmpty[A, C]
  protected def unsafeImmutableApply[A, C <: Iterable[A]](it: C)(implicit
    factory: scala.collection.Factory[A, C]
  ) : NonEmpty[A, C]

  implicit def fromRefined[K, V, CC[X, Y] <: scala.collection.Map[X, Y], F[_, _]](
    m: F[CC[K, V], refined.collection.NonEmpty]
  )(implicit
    rt: RefType[F],
    factory: scala.collection.Factory[(K, V), CC[K, V]],
  ): NonEmpty[(K, V), CC[K, V]] = unsafeImmutableApply[(K, V), CC[K, V]](rt.unwrap(m))

  implicit def fromRefined[C <: scala.collection.BitSet, F[_, _]](
    s: F[C, refined.collection.NonEmpty]
  )(implicit
    rt: RefType[F],
    factory: scala.collection.Factory[Int, C],
  ): NonEmpty[Int, C] = unsafeImmutableApply[Int, C](rt.unwrap(s))

  implicit def fromRefined[F[_, _]](
    s: F[String, refined.collection.NonEmpty]
  )(implicit
    rt: RefType[F],
    factory: scala.collection.Factory[Char, WrappedString],
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](rt.unwrap(s))
}

trait FromRefinedLowPriority {
  self: FromRefined =>

  implicit def fromRefined[A, CC[X] <: Iterable[X], F[_, _]](
    it: F[CC[A], refined.collection.NonEmpty]
  )(implicit
    rt: RefType[F],
    factory: scala.collection.Factory[A, CC[A]],
  ): NonEmpty[A, CC[A]] = unsafeImmutableApply[A, CC[A]](rt.unwrap(it))
}

trait ToRefined {
  implicit def toRefined[A, C <: Iterable[A]](
    ne: NonEmpty[A, C]
  ): refined.api.Refined[C, refined.collection.NonEmpty] =
    implicitly[RefType[refined.api.Refined]].unsafeWrap(ne.value)

  implicit def toRefined(
    ne: NonEmpty[Char, WrappedString]
  ): refined.api.Refined[String, refined.collection.NonEmpty] =
    implicitly[RefType[refined.api.Refined]].unsafeWrap(ne.value.unwrap)

  implicit def wrappedStringToRefinedOp(
    ne: NonEmpty[Char, WrappedString]
  ): ToRefined.WrappedStringToRefinedOp[WrappedString, refined.collection.NonEmpty] =
    new ToRefined.WrappedStringToRefinedOp(ne)
}
object ToRefined {
  class WrappedStringToRefinedOp[+T, P](
    private val r: Refined[T, P]
  ) extends AnyVal {
    def toRefined(implicit
      ev: T <:< WrappedString
    ): refined.api.Refined[String, refined.collection.NonEmpty] =
      implicitly[RefType[refined.api.Refined]].unsafeWrap(ev(r.value).unwrap)
  }
}
