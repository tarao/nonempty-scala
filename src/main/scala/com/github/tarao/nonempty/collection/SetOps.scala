package com.github.tarao.nonempty.collection

import scala.collection.{BuildFrom, Set => ColSet}
import scala.collection.immutable

/** Methods inherited from `Set` that preserve non-emptiness. */
trait SetOps[+A, +C <: Iterable[A]] extends Any {
  self: NonEmpty[A, C] =>

  /** Computes the union between of set and another set.
    *
    * @param   that  the set to form the union with.
    * @return  a new set consisting of all elements that are in this
    *          set or in the given set `that`.
    * @see [[scala.collection.SetOps!.union]]
    */
  @inline def union[B >: A, C2 <: Iterable[B]](that: ColSet[B])(implicit
    coll: C => ColSet[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] = concat(that)

  /** Alias for `union` */
  @inline def |[B >: A, C2 <: Iterable[B]](that: ColSet[B])(implicit
    coll: C => ColSet[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] = concat(that)

  /** Creates a new set with an additional element, unless the element
    * is already present.
    *
    * @param elem the element to be added
    * @return a new set that contains all elements of this set and that also
    *         contains `elem`.
    */
  def incl[B >: A, C2 <: Iterable[B]](elem: B)(implicit
    coll: C => immutable.Set[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(value.incl(elem)))

  /** Alias for `incl` */
  def +[B >: A, C2 <: Iterable[B]](elem: B)(implicit
    coll: C => immutable.Set[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] = incl(elem)
}
