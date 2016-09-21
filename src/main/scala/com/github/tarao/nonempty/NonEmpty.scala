package com.github.tarao
package nonempty

import scala.collection.GenIterable
import scala.language.implicitConversions

/** A value class for non-empty iterable collections.
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
  *  1. there is only two factory methods:
  *     - an implicit conversion from `Iterable[A]` to
  *       `Option[NonEmpty[A]]` where an empty value will be None
  *     - `NonEmpty.apply`, which takes at least one argument
  *
  * A value of type `NonEmpty[A]` can be used as a `Iterable[A]` by an
  * implicit conversion from `NonEmpty[A]` to `Iterable[A]`.  It also
  * has some collection methods that preserve non-emptiness.
  *
  * @define thatinfo the class of the returned collection. Where possible,
  *   `That` is the same class as the current collection class `Repr`,
  *   but this depends on the element type `B` being admissible for
  *   that class, which means that an implicit instance of type
  *   `CanBuildFrom[Repr, B, That]` is found.
  * @define bfinfo an implicit value of class `CanBuildFrom` which determines
  *   the result class `That` from the current representation type
  *   `Repr` and and the new element type `B`.
  * @define orderDependent
  *
  *   Note: might return different results for different runs, unless the
  *   underlying collection type is ordered.
  * @define willNotTerminateInf
  *
  *   Note: will not terminate for infinite-sized collections.
  * @define Coll `NonEmpty`
  * @define coll collection
  */
class NonEmpty[+A] private[nonempty] (
  private val iterable: Iterable[A]
) extends AnyVal {
  override def toString = iterable.toString

  /** Returns a new $coll containing the elements from the left hand
    *  operand followed by the elements from the right hand
    *  operand. The element type of the $coll is the most specific
    *  superclass encompassing the element types of the two operands.
    *
    * @param that   the traversable to append.
    * @tparam B     the element type of the returned collection.
    * @tparam That  $thatinfo
    * @param bf     $bfinfo
    * @return       a new collection of type `That` which contains all elements
    *               of this $coll followed by all elements of `that`.
    *
    * @usecase def ++[B](that: TraversableOnce[B]): $Coll[B]
    *   @inheritdoc
    *
    *   Example:
    *   {{{
    *     scala> val a = NonEmpty(1)
    *     a: NonEmpty[Int] = List(1)
    *
    *     scala> val b = NonEmpty(2)
    *     b: NonEmpty[Int] = List(2)
    *
    *     scala> val c = a ++ b
    *     c: NonEmpty[Int] = List(1, 2)
    *
    *     scala> val d = NonEmpty('a')
    *     d: NonEmpty[Char] = List(a)
    *
    *     scala> val e = c ++ d
    *     e: NonEmpty[AnyVal] = List(1, 2, a)
    *   }}}
    *
    *   @return       a new $coll which contains all elements of this $coll
    *                 followed by all elements of `that`.
    */
  def ++[B >: A, That](that: TraversableOnce[B])(implicit
    bf: CanBuildFrom[A, B, That]
  ): That = iterable.++(that)(bf.canBuildFrom)

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
    */
  def groupBy[K](f: A => K): scala.collection.immutable.Map[K, NonEmpty[A]] =
    iterable.groupBy(f).mapValues(new NonEmpty(_))

  /** Partitions elements in fixed size ${coll}s.
    *
    * @param  size the number of elements per group
    * @return An iterator producing ${coll}s of size `size`, except the
    *         last will be less than size `size` if the elements don't
    *         divide evenly.
    */
  def grouped(size: Int): Iterator[NonEmpty[A]] =
    iterable.grouped(size).map(new NonEmpty(_))

  /** Builds a new collection by applying a function to all elements of
    * this $coll.
    *
    * @param f      the function to apply to each element.
    * @tparam B     the element type of the returned collection.
    * @tparam That  $thatinfo
    * @param bf     $bfinfo
    * @return       a new collection of type `That` resulting from applying
    *               the given function `f` to each element of this
    *               $coll and collecting the results.
    *
    * @usecase def map[B](f: A => B): $Coll[B]
    *   @inheritdoc
    *   @return       a new $coll resulting from applying the given function
    *                 `f` to each element of this $coll and collecting
    *                 the results.
    */
  def map[B, That](f: A => B)(implicit
    bf: CanBuildFrom[A, B, That]
  ): That = iterable.map(f)(bf.canBuildFrom)

  /** Computes a prefix scan of the elements of the collection.
    *
    * Note: The neutral element `z` may be applied more than once.
    *
    * @tparam B         element type of the resulting collection
    * @tparam That      type of the resulting collection
    * @param z          neutral element for the operator `op`
    * @param op         the associative operator for the scan
    * @param bf         combiner factory which provides a combiner
    *
    * @return           a new $coll containing the prefix scan of the elements
    *                    in this $coll
    */
  def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit
    bf: CanBuildFrom[A, B, That]
  ): That = iterable.scan(z)(op)(bf.canBuildFrom)

  /** Produces a collection containing cumulative results of applying the
    * operator going left to right.
    *
    * $willNotTerminateInf
    * $orderDependent
    *
    * @tparam B      the type of the elements in the resulting collection
    * @tparam That   the actual type of the resulting collection
    * @param z       the initial value
    * @param op      the binary operator applied to the intermediate result
    *                and the element
    * @param bf      $bfinfo
    * @return        collection with intermediate results
    */
  def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit
    bf: CanBuildFrom[A, B, That]
  ): That = iterable.scanLeft(z)(op)(bf.canBuildFrom)

  /** Produces a collection containing cumulative results of applying
    * the operator going right to left.
    * The head of the collection is the last cumulative result.
    * $willNotTerminateInf
    * $orderDependent
    *
    * Example:
    * {{{
    *   NonEmpty(1, 2, 3, 4).scanRight(0)(_ + _) == NonEmpty(10, 9, 7, 4, 0)
    * }}}
    *
    * @tparam B      the type of the elements in the resulting collection
    * @tparam That   the actual type of the resulting collection
    * @param z       the initial value
    * @param op      the binary operator applied to the intermediate result
    *                and the element
    * @param bf      $bfinfo
    * @return        collection with intermediate results
    */
  def scanRight[B, That](z: B)(op: (A, B) => B)(implicit
    bf: CanBuildFrom[A, B, That]
  ): That = iterable.scanRight(z)(op)(bf.canBuildFrom)

  /** Converts this $coll of pairs into two collections of the first and
    * second half of each pair.
    *
    *    {{{
    *    val xs = NonEmpty(
    *               (1, "one"),
    *               (2, "two"),
    *               (3, "three")).unzip
    *    // xs == (NonEmpty(1, 2, 3),
    *    //        NonEmpty(one, two, three))
    *    }}}
    *
    *  @tparam A1    the type of the first half of the element pairs
    *  @tparam A2    the type of the second half of the element pairs
    *  @param asPair an implicit conversion which asserts that the element type
    *                of this $coll is a pair.
    *  @return       a pair of ${coll}s, containing the first, respectively
    *                second half of each element pair of this $coll.
    */
  def unzip[A1, A2](implicit
    asPair: A => (A1, A2)
  ): (NonEmpty[A1], NonEmpty[A2]) = {
    val (a1, a2) = iterable.unzip(asPair)
    (new NonEmpty(a1), new NonEmpty(a2))
  }

  /** Converts this $coll of triples into three collections of the
    * first, second, and third element of each triple.
    *
    *   {{{
    *   val xs = NonEmpty(
    *              (1, "one", '1'),
    *              (2, "two", '2'),
    *              (3, "three", '3')).unzip3
    *   // xs == (NonEmpty(1, 2, 3),
    *   //        NonEmpty(one, two, three),
    *   //        NonEmpty(1, 2, 3))
    *   }}}
    *
    * @tparam A1       the type of the first member of the element triples
    * @tparam A2       the type of the second member of the element triples
    * @tparam A3       the type of the third member of the element triples
    * @param asTriple  an implicit conversion which asserts that the element
    *                  type of this $coll is a triple.
    * @return          a triple of ${coll}s, containing the first, second,
    *                  respectively third member of each element
    *                  triple of this $coll.
    */
  def unzip3[A1, A2, A3](implicit
    asTriple: A => (A1, A2, A3)
  ): (NonEmpty[A1], NonEmpty[A2], NonEmpty[A3]) = {
    val (a1, a2, a3) = iterable.unzip3(asTriple)
    (new NonEmpty(a1), new NonEmpty(a2), new NonEmpty(a3))
  }

  /** Returns a $coll formed from this $coll and another iterable
    * collection by combining corresponding elements in pairs.
    * If one of the two collections is shorter than the other,
    * placeholder elements are used to extend the shorter collection
    * to the length of the longer.
    *
    * @param that     the iterable providing the second half of each result pair
    * @param thisElem the element to be used to fill up the result if this
    *                 $coll is shorter than `that`.
    * @param thatElem the element to be used to fill up the result if `that` is
    *                 shorter than this $coll.
    * @return         a new collection of type `That` containing pairs
    *                 consisting of corresponding elements of this
    *                 $coll and `that`. The length of the returned
    *                 collection is the maximum of the lengths of this
    *                 $coll and `that`. If this $coll is shorter than
    *                 `that`, `thisElem` values are used to pad the
    *                 result. If `that` is shorter than this $coll,
    *                 `thatElem` values are used to pad the result.
    *
    * @usecase def zipAll[B](that: Iterable[B], thisElem: A, thatElem: B): $Coll[(A, B)]
    *   @inheritdoc
    *
    *   $orderDependent
    *
    *   @param   that   the iterable providing the second half of each result
    *                   pair
    *   @param thisElem the element to be used to fill up the result if this
    *                   $coll is shorter than `that`.
    *   @param thatElem the element to be used to fill up the result if `that`
    *                   is shorter than this $coll.
    *   @tparam  B      the type of the second half of the returned pairs
    *   @return         a new $coll containing pairs consisting of
    *                   corresponding elements of this $coll and
    *                   `that`. The length of the returned collection
    *                   is the maximum of the lengths of this $coll
    *                   and `that`. If this $coll is shorter than
    *                   `that`, `thisElem` values are used to pad the
    *                   result. If `that` is shorter than this $coll,
    *                   `thatElem` values are used to pad the result.
    */
  def zipAll[B, A1 >: A, That](
    that: GenIterable[B],
    thisElem: A1,
    thatElem: B
  )(implicit bf: CanBuildFrom[A, (A1, B), That]): That =
    iterable.zipAll(that, thisElem, thatElem)(bf.canBuildFrom)

  /** Zips this $coll with its indices.
    *
    *  @tparam  A1    the type of the first half of the returned pairs (this
    *                 is always a supertype of the collection's
    *                 element type `A`).
    *  @tparam  That  the class of the returned collection. Where possible,
    *                 `That` is the same class as the current
    *                 collection class `Repr`, but this depends on the
    *                 element type `(A1, Int)` being admissible for
    *                 that class, which means that an implicit
    *                 instance of type `CanBuildFrom[Repr, (A1, Int),
    *                 That]`. is found.
    *  @param  bf     an implicit value of class `CanBuildFrom` which
    *                 determines the result class `That` from the
    *                 current representation type `Repr` and the new
    *                 element type `(A1, Int)`.
    *  @return        a new collection of type `That` containing pairs
    *                 consisting of all elements of this $coll paired
    *                 with their index. Indices start at `0`.
    *
    *  @usecase def zipWithIndex: $Coll[(A, Int)]
    *    @inheritdoc
    *
    *    $orderDependent
    *
    *    @return        a new $coll containing pairs consisting of all elements
    *                   of this $coll paired with their index. Indices
    *                   start at `0`.
    *    @example
    *      `NonEmpty("a", "b", "c").zipWithIndex = NonEmpty(("a", 0), ("b", 1), ("c", 2))`
    *
    */
  def zipWithIndex[A1 >: A, That](implicit
    bf: CanBuildFrom[A, (A1, Int), That]
  ): That = iterable.zipWithIndex(bf.canBuildFrom)
}
object NonEmpty {
  /** Convert a `Traversable[A]` to `Option[NonEmpty[A]]`.
    *
    * There is no way to directly convert a `Traversable[A]` into a `NonEmpty[A]`.
    */
  def fromTraversable[A](t: Traversable[A]): Option[NonEmpty[A]] = t.toIterable

  /** Convert a `Iterable[A]` to `Option[NonEmpty[A]]`.
    *
    * There is no way to directly convert a `Iterable[A]` into a `NonEmpty[A]`.
    */
  implicit def fromIterable[A](it: Iterable[A]): Option[NonEmpty[A]] =
    if (it.isEmpty) None
    else Some(new NonEmpty[A](it))

  /** Treat a `NonEmpty[A]` as a `Iterable[A]`. */
  implicit def toIterable[A](ne: NonEmpty[A]): Iterable[A] = ne.iterable

  /** Directly create a `NonEmpty[A]` by passing no less than one parameter. */
  def apply[A](head: A, elements: A*): NonEmpty[A] =
    new NonEmpty[A](head +: Seq(elements: _*))
}
