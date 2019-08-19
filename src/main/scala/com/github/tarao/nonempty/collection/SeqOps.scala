package com.github.tarao.nonempty.collection

import scala.collection.BuildFrom

/** Methods inherited from `Iterable` that preserve non-emptiness.
  *
  * @define willNotTerminateInf
  *
  *              Note: will not terminate for infinite-sized collections.
  * @define willForceEvaluation
  *              Note: Even when applied to a view or a lazy
  *              collection it will always force the elements.
  * @define coll collection
  */
trait SeqOps[+A, +C <: Iterable[A]] extends Any {
  self: NonEmpty[A, C] =>

  /** A copy of the $coll with an element prepended.
    *
    * Also, the original $coll is not modified, so you will want to
    * capture the result.
    *
    * @param  elem   the prepended element
    * @tparam B      the element type of the returned $coll.
    *
    * @return a new $coll consisting of `value` followed
    *         by all elements of this $coll.
    * @see [[scala.collection.SeqOps!.prepended]]
    */
  def prepended[B >: A, C2 <: Iterable[B]](elem: B)(implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(coll(value).prepended(elem)))

  /** Alias for `prepended`.
    *
    * Note that :-ending operators are right associative (see example).
    * A mnemonic for `+:` vs. `:+` is: the colon goes on the collection side.
    *
    * @see [[#prepended]]
    * @see [[scala.collection.SeqOps!.+:]]
    */
  @inline def +:[B >: A, C2 <: Iterable[B]](elem: B)(implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] = prepended(elem)

  /** A copy of this $coll with an element appended.
    *
    * $willNotTerminateInf
    *
    * @param  elem   the appended element
    * @tparam B      the element type of the returned $coll.
    * @return a new $coll consisting of
    *         all elements of this $coll followed by `value`.
    * @see [[scala.collection.SeqOps!.appended]]
    */
  def appended[B >: A, C2 <: Iterable[B]](elem: B)(implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(coll(value).appended(elem)))

  /** Alias for `appended`
    *
    * Note that :-ending operators are right associative (see example).
    * A mnemonic for `+:` vs. `:+` is: the colon goes on the collection side.
    *
    * @see [[#appended]]
    * @see [[scala.collection.SeqOps!.:+]]
    */
  @inline def :+[B >: A, C2 <: Iterable[B]](elem: B)(implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] = appended(elem)

  /** As with `:++`, returns a new collection containing the elements
    * from the left operand followed by the elements from the right
    * operand.
    *
    * It differs from `:++` in that the right operand determines the type of
    * the resulting collection rather than the left one.
    * Mnemonic: the colon is on the side of the new collection type.
    *
    * @param prefix the iterable to prepend.
    * @tparam B     the element type of the returned collection.
    * @return       a new $coll which contains all elements of `prefix` followed
    *               by all the elements of this $coll.
    * @see [[scala.collection.SeqOps!.prependedAll]]
    */
  def prependedAll[B >: A, C2 <: Iterable[B]](prefix: IterableOnce[B])(implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(coll(value).prependedAll(prefix)))

  /** Alias for `prependedAll`
    * @see [[#prependedAll]]
    * @see [[scala.collection.SeqOps!.++:]]
    */
  @inline def ++:[B >: A, C2 <: Iterable[B]](prefix: IterableOnce[B])(implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] = prependedAll(prefix)

  /** Returns a new $coll containing the elements from the left hand
    * operand followed by the elements from the right hand
    * operand. The element type of the $coll is the most specific
    * superclass encompassing the element types of the two operands.
    *
    * @param suffix the iterable to append.
    * @tparam B     the element type of the returned collection.
    * @return       a new collection of type `NonEmpty[B, C2]` which contains all elements
    *               of this $coll followed by all elements of `suffix`.
    * @see [[scala.collection.SeqOps!.appendedAll]]
    */
  def appendedAll[B >: A, C2 <: Iterable[B]](prefix: IterableOnce[B])(implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(coll(value).appendedAll(prefix)))

  /** Alias for `appendedAll`
    * @see [[#appendedAll]]
    * @see [[scala.collection.SeqOps!.:++]]
    */
  @inline def :++[B >: A, C2 <: Iterable[B]](prefix: IterableOnce[B])(implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] = appendedAll(prefix)

  /** Iterates over distinct permutations.
    *
    * $willForceEvaluation
    *
    * @return   An Iterator which traverses the distinct permutations of this $coll.
    * @example  `"abb".permutations = Iterator(abb, bab, bba)`
    * @see [[scala.collection.SeqOps!.permutations]]
    */
  def permutations[B >: A, C2 <: Iterable[B]](implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): Iterator[NonEmpty[B, C2]] = coll(value).permutations.map { v =>
    unsafeApply[B, C2](bf.fromSpecific(value)(v))
  }

  /** Returns new $coll with elements in reversed order.
    *
    * $willNotTerminateInf
    * $willForceEvaluation
    *
    * @return A new $coll with all elements of this $coll in reversed order.
    * @see [[scala.collection.SeqOps!.reverse]]
    */
  def reverse[B >: A, C2 <: Iterable[B]](implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(coll(value).reverse))

  /** Sorts this $coll according to the Ordering which results from
    * transforming an implicitly given Ordering with a transformation
    * function.
    * $willNotTerminateInf
    * $willForceEvaluation
    *
    * The sort is stable. That is, elements that are equal (as
    * determined by `ord.compare`) appear in the same order in the
    * sorted sequence as in the original.
    *
    * @see [[scala.math.Ordering]]
    * @param   f the transformation function mapping elements
    *          to some other domain `A2`.
    * @param   ord the ordering assumed on domain `A2`.
    * @tparam  B the target type of the transformation `f`, and the type where
    *          the ordering `ord` is defined.
    * @return  a $coll consisting of the elements of this $coll
    *          sorted according to the ordering where `x < y` if
    *          `ord.lt(f(x), f(y))`.
    * @see [[scala.collection.SeqOps!.sortBy]]
    */
  def sortBy[A2, B >: A, C2 <: Iterable[B]](f: A => A2)(implicit
    ord: Ordering[A2],
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(value.toSeq.sortBy(f)(ord)))

  /** Sorts this $coll according to a comparison function.
    * $willNotTerminateInf
    * $willForceEvaluation
    *
    * The sort is stable. That is, elements that are equal (as determined by
    * `lt`) appear in the same order in the sorted sequence as in the original.
    *
    * @param  lt  the comparison function which tests whether
    *             its first argument precedes its second argument in
    *             the desired ordering.
    * @return     a $coll consisting of the elements of this $coll
    *             sorted according to the comparison function `lt`.
    * @see [[scala.collection.SeqOps!.sortWith]]
    */
  def sortWith[B >: A, C2 <: Iterable[B]](lt: (A, A) => Boolean)(implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(value.toSeq.sortWith(lt)))

  /** Sorts this $coll according to an Ordering.
    *
    * The sort is stable. That is, elements that are equal (as
    * determined by `ord.compare`) appear in the same order in the
    * sorted sequence as in the original.
    *
    * @see [[scala.math.Ordering]]
    *
    * $willForceEvaluation
    *
    * @param  ord the ordering to be used to compare elements.
    * @return     a $coll consisting of the elements of this $coll
    *             sorted according to the ordering `ord`.
    * @see [[scala.collection.SeqOps!.sorted]]
    */
  def sorted[B >: A, C2 <: Iterable[B]](implicit
    ord: Ordering[B],
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(coll(value).sorted(ord)))

  /** A copy of this $coll with one single replaced element.
    * @param  index  the position of the replacement
    * @param  elem   the replacing element
    * @tparam B        the element type of the returned $coll.
    * @return a new $coll which is a copy of this $coll with the element at position `index` replaced by `elem`.
    * @throws scala.IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`. In case of a
    *                                   lazy collection this exception may be thrown at a later time or not at
    *                                   all (if the end of the collection is never evaluated).
    * @see [[scala.collection.SeqOps!.updated]]
    */
  def updated[B >: A, C2 <: Iterable[B]](index: Int, elem: B)(implicit
    coll: C => scala.collection.Seq[B],
    bf: BuildFrom[C, B, C2],
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(coll(value).updated(index, elem)))

}
