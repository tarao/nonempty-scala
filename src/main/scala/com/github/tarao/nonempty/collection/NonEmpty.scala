package com.github.tarao.nonempty
package collection

import eu.timepit.refined

/** A value class for non-empty collections.
  *
  * It is intended to be used as a parameter type to restrict a
  * collection value to be non-empty.  This restriction is guaranteed
  * statically, i.e., it is not possible to pass an empty collection
  * as a parameter of type `NonEmpty[]`.
  *
  * The static safety is guaranteed by three reasons:
  *  1. there is no way to create a new instance of `NonEmpty[]` other
  *     than by factory methods provided by singleton object
  *     `NonEmpty`
  *  1. there are only three factory methods:
  *     - `NonEmpty.from`, which is also an implicit conversion, from
  *       `C <: Iterable[A]` to `Option[NonEmpty[A, C]]` where an
  *       empty value will be None
  *     - `NonEmpty.fromRefined`, which is also an implicit
  *       conversion, from `eu.timepit.refined.Refined[C,
  *       eu.timepit.refined.collection.NonEmpty]` to `NonEmpty[A, C]`
  *       for some `C <: Iterable[A]`.
  *     - `NonEmpty.apply`, which takes at least one argument
  *  1. the conversions clone the instance of the collection if it is
  *     mutable (or otherwise its elements can be dropped and it
  *     becomes empty)
  *
  * A value of type `NonEmpty[A, C]` can be used as a `C` by an
  * implicit conversion from `NonEmpty[A, C]` to `C`.  It also has
  * some collection methods that preserve non-emptiness.
  *
  * @tparam A the element type of the collection
  * @tparam C the collection type
  */
final class NonEmpty[+A, +C <: Iterable[A]] private (val value: C)
    extends AnyVal
    with IterableOps[A, C]
    with SeqOps[A, C]
    with ListOps[A, C]
    with MapOps[A, C]
    with SetOps[A, C]
    with Refined[C, refined.collection.NonEmpty] {
  @inline protected def unsafeApply[A, C <: Iterable[A]](
    it: C
  ) : NonEmpty[A, C] = NonEmpty.unsafeApply[A, C](it)

  override def toString = value.toString
}
object NonEmpty extends AnyRef
    with MapOps.Implicits
    with FromIterable with ToIterable
    with FromRefined with ToRefined {
  @inline override protected def unsafeApply[A, C <: Iterable[A]](it: C) : NonEmpty[A, C] =
    new NonEmpty[A, C](it)

  @inline override protected def unsafeImmutableApply[A, C <: Iterable[A]](it: C)(implicit
    factory: scala.collection.Factory[A, C]
  ) : NonEmpty[A, C] = it match {
    case _: scala.collection.mutable.Iterable[_] =>
      unsafeApply[A, C](factory.newBuilder.addAll(it).result)
    case _ =>
      unsafeApply[A, C](it)
  }

  /** Create a `NonEmpty` instance by providing at least one element.
    *
    * Example {{{
    *   val ne = NonEmpty[List[Int]](1, 2, 3)
    * }}}
    *
    * @tparam C the collection type
    */
  def apply[C]: Builder[C] = new Builder[C]

  class Builder[C] {
    def apply[A, C1 <: C with Iterable[A]](head: A, elements: A*)(implicit
      factory: scala.collection.Factory[A, C1]
    ): NonEmpty[A, C1] =
      unsafeImmutableApply[A, C1](factory.fromSpecific(head +: elements))
  }
}
