package com.github.tarao.nonempty
package collection

import eu.timepit.refined
import scala.collection.immutable

/** A value class for non-empty collections.
  *
  * It is intended to be used as a parameter type to restrict a
  * collection value to be non-empty.  This restriction is guaranteed
  * statically, i.e., it is not possible to pass an empty collection
  * as a parameter of type `NonEmpty[]`.
  *
  * The static safety is guaranteed by two reasons:
  *  1. there is no way to create a new instance of `NonEmpty[]` other
  *     than by factory methods provided by singleton object
  *     `NonEmpty`
  *  1. there are only three factory methods:
  *     - `NonEmpty.from`, which is also an implicit conversion, from
  *       `C <: Iterable[A]` to `Option[NonEmpty[A, C]]` where an
  *       empty or mutable value will be None
  *     - `NonEmpty.fromRefined`, which is also an implicit
  *       conversion, from `eu.timepit.refined.Refined[C,
  *       eu.timepit.refined.collection.NonEmpty]` to `NonEmpty[A, C]`
  *       for some `C <: Iterable[A]`.
  *     - `NonEmpty.apply`, which takes at least one argument
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
    with MapOps[A, C]
    with SetOps[A, C]
    with ListOps[A, C]
    with QueueOps[A, C]
    with LazyListOps[A, C]
    with StringOps[A, C]
    with Refined[C, refined.collection.NonEmpty] {
  @inline protected def unsafeApply[A, C <: Iterable[A]](
    it: C
  ) : NonEmpty[A, C] = NonEmpty.unsafeApply[A, C](it)

  override def toString = value.toString
}
object NonEmpty extends AnyRef
    with MapOps.Implicits
    with LazyListOps.Implicits
    with FromIterable with ToIterable
    with FromRefined with ToRefined {
  @inline override protected def unsafeApply[A, C <: Iterable[A]](it: C) : NonEmpty[A, C] =
    new NonEmpty[A, C](it)

  @inline override protected def unsafeImmutableApply[A, C <: Iterable[A]](
    it: C
  ): Option[NonEmpty[A, C]] = it match {
    case _: immutable.Iterable[_] =>
      Some(unsafeApply[A, C](it))
    case _ =>
      // We are disallowing mutable collection to be passed because it
      // may be changed to be empty somewhere beyond the protection of
      // NonEmpty[_, _].
      //
      // It would be nice if we can convert a mutable collection such
      // as `scala.collection.mutable.ArrayBuffer` into an immutable
      // one such as `scala.collection.immutable.Seq`.  In this case,
      // we need `C` to be a common super type of the both mutable and
      // immutable types, such as `scala.collection.Seq`.  We leave
      // them as future work since they are non-trivial to achieve.
      None
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
    def apply[A, C1 <: C with immutable.Iterable[A]](
      head: A,
      elements: A*
    )(implicit factory: scala.collection.Factory[A, C1]): NonEmpty[A, C1] =
      unsafeApply[A, C1](factory.fromSpecific(head +: elements))
  }
}
