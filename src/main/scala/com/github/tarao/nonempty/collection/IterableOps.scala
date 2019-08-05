package com.github.tarao.nonempty.collection

import scala.collection.BuildFrom
import scala.collection.immutable
import scala.language.higherKinds

/** Methods inherited from `Iterable` that preserve non-emptiness.
  *
  * @define orderDependent
  *
  *              Note: might return different results for different
  *              runs, unless the underlying collection type is
  *              ordered.
  * @define willNotTerminateInf
  *
  *              Note: will not terminate for infinite-sized collections.
  * @define willForceEvaluation
  *              Note: Even when applied to a view or a lazy
  *              collection it will always force the elements.
  * @define consumesAndProducesIterator
  *              After calling this method, one should discard the
  *              iterator it was called on, and use only the iterator
  *              that was returned. Using the old iterator is
  *              undefined, subject to change, and may result in
  *              changes to the new iterator as well.
  * @define Coll `NonEmpty`
  * @define coll collection
  */
trait IterableOps[+A, +C <: Iterable[A]] extends Any {
  self: NonEmpty[A, C] =>

  /** Returns a new $coll containing the elements from the left hand
    * operand followed by the elements from the right hand
    * operand. The element type of the $coll is the most specific
    * superclass encompassing the element types of the two operands.
    *
    * @param suffix  the iterable to append.
    * @tparam B      the element type of the returned collection.
    * @return        a new $coll which contains all elements
    *                of this $coll followed by all elements of `suffix`.
    * @see [[scala.collection.IterableOps!.concat]]
    */
  def concat[B >: A, C2 <: Iterable[B]](suffix: IterableOnce[B])(implicit
    bf: BuildFrom[C, B, C2]
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(value.concat(suffix)))

  /** Alias for `concat`
    * @see [[#concat]]
    * @see [[scala.collection.IterableOps!.++]]
    */
  @inline def ++[B >: A, C2 <: Iterable[B]](suffix: IterableOnce[B])(implicit
    bf: BuildFrom[C, B, C2]
  ): NonEmpty[B, C2] = concat[B, C2](suffix)

  /** Partitions this $coll into a map of ${coll}s according to some
    * discriminator function.
    *
    * @param f     the discriminator function.
    * @tparam K    the type of keys returned by the discriminator function.
    * @return      A map from keys to ${coll}s such that the following
    *              invariant holds:
    *              {{{
    *                (xs groupBy f)(k) = xs filter (x => f(x) == k)
    *              }}}
    *              That is, every key `k` is bound to a $coll of those elements `x`
    *              for which `f(x)` equals `k`.
    * @see [[scala.collection.IterableOps!.groupBy]]
    */
  def groupBy[K, B >: A, C2 <: Iterable[B]](f: A => K)(implicit
    bf: BuildFrom[C, B, C2]
  ): NonEmpty[(K, NonEmpty[B, C2]), immutable.Map[K, NonEmpty[B, C2]]] =
    unsafeApply[(K, NonEmpty[B, C2]), immutable.Map[K, NonEmpty[B, C2]]](
      value.groupBy(f).map { case (k, v) =>
        k -> unsafeApply[B, C2](bf.fromSpecific(value)(v))
      }
    )

  /** Partitions this $coll into a map of ${coll}s according to a
    * discriminator function `key`.  Each element in a group is
    * transformed into a value of type `B` using the `value` function.
    *
    * It is equivalent to `groupBy(key).mapValues(_.map(f))`, but more
    * efficient.
    *
    * $willForceEvaluation
    *
    * @param key the discriminator function
    * @param f the element transformation function
    * @tparam K the type of keys returned by the discriminator function
    * @tparam B the type of values returned by the transformation function
    * @see [[scala.collection.IterableOps!.groupMap]]
    */
  def groupMap[K, B, C2 <: Iterable[B]](key: A => K)(f: A => B)(implicit
    bf: BuildFrom[C, B, C2]
  ): NonEmpty[(K, NonEmpty[B, C2]), immutable.Map[K, NonEmpty[B, C2]]] =
    unsafeApply[(K, NonEmpty[B, C2]), immutable.Map[K, NonEmpty[B, C2]]](
      value.groupMap(key)(f).map { case (k, v) =>
        k -> unsafeApply[B, C2](bf.fromSpecific(value)(v))
      }
    )

  /** Partitions this $coll into a map according to a discriminator
    * function `key`. All the values that have the same discriminator
    * are then transformed by the `value` function and then reduced
    * into a single value with the `reduce` function.
    *
    * It is equivalent to
    * `groupBy(key).mapValues(_.map(f).reduce(reduce))`, but more
    * efficient.
    *
    * $willForceEvaluation
    * @see [[scala.collection.IterableOps!.groupMapReduce]]
    */
  def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): NonEmpty[(K, B), immutable.Map[K, B]] =
    unsafeApply[(K, B), immutable.Map[K, B]](
      value.groupMapReduce(key)(f)(reduce)
    )

  /** Partitions elements in fixed size ${coll}s.
    *
    * @param  size the number of elements per group
    * @return An iterator producing ${coll}s of size `size`, except the
    *         last will be less than size `size` if the elements don't
    *         divide evenly.
    * @see [[scala.collection.IterableOps!.grouped]]
    */
  def grouped[B >: A, C2 <: Iterable[B]](size: Int)(implicit
    bf: BuildFrom[C, B, C2],
  ): Iterator[NonEmpty[B, C2]] =
    value.grouped(size).map { v =>
      unsafeApply[B, C2](bf.fromSpecific(value)(v))
    }

  /** Builds a new $coll by applying a function to all elements of this
    * $coll.
    *
    * @param f      the function to apply to each element.
    * @tparam B     the element type of the returned $coll.
    * @return       a new $coll resulting from applying the given function
    *               `f` to each element of this $coll and collecting the results.
    * @note    Reuse: $consumesAndProducesIterator
    * @see [[scala.collection.IterableOps!.map]]
    */
  def map[B, C2 <: Iterable[B]](f: A => B)(implicit
    bf: BuildFrom[C, B, C2]
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(value.map(f)))

  /** Computes a prefix scan of the elements of the collection.
    *
    * Note: The neutral element `z` may be applied more than once.
    *
    * @tparam B         element type of the resulting collection
    * @param z          neutral element for the operator `op`
    * @param op         the associative operator for the scan
    *
    * @return           a new $coll containing the prefix scan of the elements in this $coll
    * @see [[scala.collection.IterableOps!.scan]]
    */
  def scan[B >: A, C2 <: Iterable[B]](z: B)(op: (B, B) => B)(implicit
    bf: BuildFrom[C, B, C2]
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(value.scan(z)(op)))

  /** Produces a $coll containing cumulative results of applying the
    * operator going left to right, including the initial value.
    *
    * $willNotTerminateInf
    * $orderDependent
    *
    * @tparam B      the type of the elements in the resulting collection
    * @param z       the initial value
    * @param op      the binary operator applied to the intermediate result and the element
    * @return        collection with intermediate results
    * @note          Reuse: $consumesAndProducesIterator
    * @see [[scala.collection.IterableOps!.scanLeft]]
    */
  def scanLeft[B, C2 <: Iterable[B]](z: B)(op: (B, A) => B)(implicit
    bf: BuildFrom[C, B, C2]
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(value.scanLeft(z)(op)))

  /** Produces a collection containing cumulative results of applying
    * the operator going right to left.  The head of the collection
    * is the last cumulative result.  $willNotTerminateInf
    * $orderDependent $willForceEvaluation
    *
    * @tparam B      the type of the elements in the resulting collection
    * @param z       the initial value
    * @param op      the binary operator applied to the intermediate result and the element
    * @return        collection with intermediate results
    * @see [[scala.collection.IterableOps!.scanRight]]
    */
  def scanRight[B, C2 <: Iterable[B]](z: B)(op: (A, B) => B)(implicit
    bf: BuildFrom[C, B, C2]
  ): NonEmpty[B, C2] =
    unsafeApply[B, C2](bf.fromSpecific(value)(value.scanRight(z)(op)))

  /** Groups elements in fixed size blocks by passing a "sliding window"
    * over them (as opposed to partitioning them, as is done in
    * `grouped`.)  The "sliding window" step is set to one.
    * @see [[scala.collection.Iterator]], method `sliding`
    *
    * @param size the number of elements per group
    * @return An iterator producing ${coll}s of size `size`, except the
    *         last element (which may be the only element) will be truncated
    *         if there are fewer than `size` elements remaining to be grouped.
    * @see [[scala.collection.IterableOps!.sliding(size:Int)*]]
    */
  def sliding[B >: A, C2 <: Iterable[B]](size: Int)(implicit
    bf: BuildFrom[C, B, C2]
  ): Iterator[NonEmpty[B, C2]] =
    value.sliding(size).map(v => unsafeApply[B, C2](bf.fromSpecific(value)(v)))

  /** Groups elements in fixed size blocks by passing a "sliding window"
    * over them (as opposed to partitioning them, as is done in grouped.)
    * @see [[scala.collection.Iterator]], method `sliding`
    *
    * @param size the number of elements per group
    * @param step the distance between the first elements of successive
    *        groups
    * @return An iterator producing ${coll}s of size `size`, except the
    *         last element (which may be the only element) will be truncated
    *         if there are fewer than `size` elements remaining to be grouped.
    * @see [[scala.collection.IterableOps!.sliding(size:Int,step:Int)*]]
    */
  def sliding[B >: A, C2 <: Iterable[B]](size: Int, step: Int)(implicit
    bf: BuildFrom[C, B, C2]
  ): Iterator[NonEmpty[B, C2]] =
    value.sliding(size, step).map(v => unsafeApply[B, C2](bf.fromSpecific(value)(v)))

  /** Transposes this $coll of iterable collections into
    * a $coll of ${coll}s.
    *
    * The resulting collection's type will be guided by the
    * static type of $coll.
    *
    * $willForceEvaluation
    *
    * @tparam B the type of the elements of each iterable collection.
    * @param  asIterable an implicit conversion which asserts that the
    *         element type of this $coll is an `Iterable`.
    * @return a two-dimensional $coll of ${coll}s which has as ''n''th row
    *         the ''n''th column of this $coll.
    * @throws scala.IllegalArgumentException if all collections in this $coll
    *         are not of the same size.
    * @see [[scala.collection.IterableOps!.transpose]]
    */
  def transpose[A1 >: A, B, CC[X] <: Iterable[X]](implicit
    asIterable: A1 => Iterable[B],
    coll: C <:< CC[A1],
    bf1: BuildFrom[C, B, CC[B]],
    bf2: BuildFrom[C, NonEmpty[B, CC[B]], CC[NonEmpty[B, CC[B]]]]
  ): CC[NonEmpty[B, CC[B]]]  =
    bf2.fromSpecific(value)(coll(value).transpose.map { v =>
      unsafeApply[B, CC[B]](bf1.fromSpecific(value)(v))
    })

  /** Converts this $coll of pairs into two collections of the first and
    * second half of each pair.
    *
    * @tparam A1    the type of the first half of the element pairs
    * @tparam A2    the type of the second half of the element pairs
    * @param asPair an implicit conversion which asserts that the element type
    *               of this $coll is a pair.
    * @return       a pair of ${coll}s, containing the first, respectively
    *               second half of each element pair of this $coll.
    * @see [[scala.collection.IterableOps!.unzip]]
    */
  def unzip[A1, A2, B >: A, CC[X] <: Iterable[X]](implicit
    asPair: A => (A1, A2),
    coll: C => CC[B],
    bf1: BuildFrom[CC[B], A1, CC[A1]],
    bf2: BuildFrom[CC[B], A2, CC[A2]],
  ): (NonEmpty[A1, CC[A1]], NonEmpty[A2, CC[A2]]) = {
    val (a1, a2) = value.unzip(asPair)
    (
      unsafeApply[A1, CC[A1]](bf1.fromSpecific(coll(value))(a1)),
      unsafeApply[A2, CC[A2]](bf2.fromSpecific(coll(value))(a2)),
    )
  }

  /** Converts this $coll of triples into three collections of the
    * first, second, and third element of each triple.
    *
    * @tparam A1       the type of the first member of the element triples
    * @tparam A2       the type of the second member of the element triples
    * @tparam A3       the type of the third member of the element triples
    * @param asTriple  an implicit conversion which asserts that the element
    *                  type of this $coll is a triple.
    * @return          a triple of ${coll}s, containing the first, second,
    *                  respectively third member of each element
    *                  triple of this $coll.
    * @see [[scala.collection.IterableOps!.unzip3]]
    */
  def unzip3[A1, A2, A3, B >: A, CC[X] <: Iterable[X]](implicit
    asTriple: A => (A1, A2, A3),
    coll: C => CC[B],
    bf1: BuildFrom[CC[B], A1, CC[A1]],
    bf2: BuildFrom[CC[B], A2, CC[A2]],
    bf3: BuildFrom[CC[B], A3, CC[A3]],
  ): (NonEmpty[A1, CC[A1]], NonEmpty[A2, CC[A2]], NonEmpty[A3, CC[A3]]) = {
    val (a1, a2, a3) = value.unzip3(asTriple)
    (
      unsafeApply[A1, CC[A1]](bf1.fromSpecific(coll(value))(a1)),
      unsafeApply[A2, CC[A2]](bf2.fromSpecific(coll(value))(a2)),
      unsafeApply[A3, CC[A3]](bf3.fromSpecific(coll(value))(a3)),
    )
  }

  /** Returns a $coll formed from this $coll and another iterable
    * collection by combining corresponding elements in pairs.  If one
    * of the two collections is shorter than the other, placeholder
    * elements are used to extend the shorter collection to the length
    * of the longer.
    *
    * @param that     the iterable providing the second half of each result pair
    * @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
    * @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
    * @return        a new collection of type `That` containing pairs consisting of
    *                corresponding elements of this $coll and `that`. The length
    *                of the returned collection is the maximum of the lengths of this $coll and `that`.
    *                If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
    *                If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
    * @see [[scala.collection.IterableOps!.zipAll]]
    */
  def zipAll[A1 >: A, B, CC[X] <: Iterable[X]](
    that: Iterable[B],
    thisElem: A1,
    thatElem: B,
  )(implicit
    coll: C => CC[A1],
    bf: BuildFrom[CC[A1], (A1, B), CC[(A1, B)]],
  ): NonEmpty[(A1, B), CC[(A1, B)]] = {
    val v = bf.fromSpecific(coll(value))(value.zipAll(that, thisElem, thatElem))
    unsafeApply[(A1, B), CC[(A1, B)]](v)
  }

  /** Zips this $coll with its indices.
    *
    * @return        A new $coll containing pairs consisting of all elements of this $coll paired with their index.
    *                Indices start at `0`.
    * @note    Reuse: $consumesAndProducesIterator
    * @see [[scala.collection.IterableOps!.zipWithIndex]]
    */
  def zipWithIndex[B >: A, CC[X] <: Iterable[X]](implicit
    coll: C => CC[B],
    bf: BuildFrom[CC[B], (B, Int), CC[(B, Int)]],
  ): NonEmpty[(B, Int), CC[(B, Int)]] = {
    val v = bf.fromSpecific(coll(value))(value.zipWithIndex)
    unsafeApply[(B, Int), CC[(B, Int)]](v)
  }
}
