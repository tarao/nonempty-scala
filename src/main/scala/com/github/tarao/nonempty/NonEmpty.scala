package com.github.tarao
package nonempty

import eu.timepit.refined
import eu.timepit.refined.api.RefType
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable
import scala.collection.immutable.LinearSeq
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.macros.blackbox

/** A value class for non-empty iterable collections.
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
  *  1. there is only two factory methods:
  *     - an implicit conversion from `Iterable[A]` to
  *       `Option[NonEmpty[A]]` where an empty value will be None
  *     - `NonEmpty.apply`, which takes at least one argument
  *  1. the implicit conversion copies the elements of a mutable
  *     collection in case the collection drops the elements
  *
  * A value of type `NonEmpty[A]` can be used as a `Iterable[A]` by an
  * implicit conversion from `NonEmpty[A]` to `Iterable[A]`.  It also
  * has some collection methods that preserve non-emptiness.
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
class NonEmpty[+A] private[nonempty] (private[nonempty] val value: Iterable[A])
    extends AnyVal
    with NonEmpty.Refined[Iterable[A], refined.collection.NonEmpty] {
  override def toString = value.toString

  /** Returns a new $coll containing the elements from the left hand
    * operand followed by the elements from the right hand
    * operand. The element type of the $coll is the most specific
    * superclass encompassing the element types of the two operands.
    *
    * @param suffix  the iterable to append.
    * @tparam B      the element type of the returned collection.
    * @return        a new $coll which contains all elements
    *                of this $coll followed by all elements of `suffix`.
    */
  def concat[B >: A](suffix: IterableOnce[B]): NonEmpty[B] =
    new NonEmpty(value.concat(suffix))

  /** Alias for `concat` */
  @inline def ++[B >: A](suffix: IterableOnce[B]): NonEmpty[B] = concat(suffix)

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
  def groupBy[K](f: A => K): immutable.Map[K, NonEmpty[A]] =
    value.groupBy(f).view.mapValues(new NonEmpty(_)).to(Map)

  /**
    * Partitions this $coll into a map of ${coll}s according to a
    * discriminator function `key`.  Each element in a group is
    * transformed into a value of type `B` using the `value` function.
    *
    * It is equivalent to `groupBy(key).mapValues(_.map(f))`, but more
    * efficient.
    *
    * {{{
    *   case class User(name: String, age: Int)
    *
    *   def namesByAge(users: Seq[User]): Map[Int, Seq[String]] =
    *     users.groupMap(_.age)(_.name)
    * }}}
    *
    * $willForceEvaluation
    *
    * @param key the discriminator function
    * @param f the element transformation function
    * @tparam K the type of keys returned by the discriminator function
    * @tparam B the type of values returned by the transformation function
    */
  def groupMap[K, B](key: A => K)(f: A => B): immutable.Map[K, NonEmpty[B]] =
    value.groupMap(key)(f).view.mapValues(new NonEmpty(_)).to(Map)

  /** Partitions elements in fixed size ${coll}s.
    *
    * @param  size the number of elements per group
    * @return An iterator producing ${coll}s of size `size`, except the
    *         last will be less than size `size` if the elements don't
    *         divide evenly.
    */
  def grouped(size: Int): Iterator[NonEmpty[A]] =
    value.grouped(size).map(new NonEmpty(_))

  /** Builds a new $coll by applying a function to all elements of this
    * $coll.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned $coll.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def map[B](f: A => B): NonEmpty[B] = new NonEmpty(value.map(f))

  /** Computes a prefix scan of the elements of the collection.
    *
    *  Note: The neutral element `z` may be applied more than once.
    *
    *  @tparam B         element type of the resulting collection
    *  @param z          neutral element for the operator `op`
    *  @param op         the associative operator for the scan
    *
    *  @return           a new $coll containing the prefix scan of the elements in this $coll
    */
  def scan[B >: A](z: B)(op: (B, B) => B): NonEmpty[B] =
    new NonEmpty(value.scan(z)(op))

  /** Produces a $coll containing cumulative results of applying the
    * operator going left to right, including the initial value.
    *
    *  $willNotTerminateInf
    *  $orderDependent
    *
    *  @tparam B      the type of the elements in the resulting collection
    *  @param z       the initial value
    *  @param op      the binary operator applied to the intermediate result and the element
    *  @return        collection with intermediate results
    *  @note          Reuse: $consumesAndProducesIterator
    */
  def scanLeft[B](z: B)(op: (B, A) => B): NonEmpty[B] =
    new NonEmpty(value.scanLeft(z)(op))

  /** Produces a collection containing cumulative results of applying
    *  the operator going right to left.  The head of the collection
    *  is the last cumulative result.  $willNotTerminateInf
    *  $orderDependent $willForceEvaluation
    *
    *  Example:
    *  {{{
    *    List(1, 2, 3, 4).scanRight(0)(_ + _) == List(10, 9, 7, 4, 0)
    *  }}}
    *
    *  @tparam B      the type of the elements in the resulting collection
    *  @param z       the initial value
    *  @param op      the binary operator applied to the intermediate result and the element
    *  @return        collection with intermediate results
    */
  def scanRight[B](z: B)(op: (A, B) => B): NonEmpty[B] =
    new NonEmpty(value.scanRight(z)(op))

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
    val (a1, a2) = value.unzip(asPair)
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
    val (a1, a2, a3) = value.unzip3(asTriple)
    (new NonEmpty(a1), new NonEmpty(a2), new NonEmpty(a3))
  }

  /** Returns a $coll formed from this $coll and another iterable collection
    *  by combining corresponding elements in pairs.
    *  If one of the two collections is shorter than the other,
    *  placeholder elements are used to extend the shorter collection to the length of the longer.
    *
    *  @param that     the iterable providing the second half of each result pair
    *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
    *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
    *  @return        a new collection of type `That` containing pairs consisting of
    *                 corresponding elements of this $coll and `that`. The length
    *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
    *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
    *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
    */
  def zipAll[A1 >: A, B](
    that: Iterable[B],
    thisElem: A1,
    thatElem: B,
  ): NonEmpty[(A1, B)] = new NonEmpty(value.zipAll(that, thisElem, thatElem))

  /** Zips this $coll with its indices.
    *
    *  @return        A new $coll containing pairs consisting of all elements of this $coll paired with their index.
    *                 Indices start at `0`.
    *  @example
    *    `List("a", "b", "c").zipWithIndex == List(("a", 0), ("b", 1), ("c", 2))`
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def zipWithIndex: NonEmpty[(A @uncheckedVariance, Int)] =
    new NonEmpty(value.zipWithIndex)
}
object NonEmpty {
  private[this] def unsafeApply[A](it: Iterable[A]): NonEmpty[A] =
    new NonEmpty(it match {
      case _: IndexedSeq[_] => it.toIndexedSeq
      case _: LinearSeq[_] | _: Set[_] | _: Map[_, _] => it
      case _ => it.toList
    })

  /** Convert a `Traversable[A]` to `Option[NonEmpty[A]]`.
    *
    * There is no way to directly convert a `Traversable[A]` into a `NonEmpty[A]`.
    */
  @deprecated
  def fromTraversable[A](t: Traversable[A]): Option[NonEmpty[A]] = t.toIterable

  /** Convert a `Iterable[A]` to `Option[NonEmpty[A]]`.
    *
    * There is no way to directly convert a `Iterable[A]` into a
    * `NonEmpty[A]`.
    *
    * If you pass a mutable collection as `it`, its elements are
    * copied.
    */
  implicit def fromIterable[A](it: Iterable[A]): Option[NonEmpty[A]] =
    Some(it).filter(_.nonEmpty).map(unsafeApply(_))

  /** Treat a `NonEmpty[A]` as a `Iterable[A]`. */
  implicit def toIterable[A](ne: NonEmpty[A]): Iterable[A] = ne.value

  /** Directly create a `NonEmpty[A]` by passing no less than one parameter. */
  def apply[A](head: A, elements: A*): NonEmpty[A] =
    new NonEmpty[A](head +: Seq(elements: _*))

  // Compatibility with refined

  implicit def fromRefined[A, L[X] <: Iterable[X], F[_, _]](
    it: F[L[A], refined.collection.NonEmpty]
  )(implicit
    rt: RefType[F]
  ): NonEmpty[A] = unsafeApply(rt.unwrap(it))

  implicit def toRefined[A](
    ne: NonEmpty[A]
  ): refined.api.Refined[Iterable[A], refined.collection.NonEmpty] =
    implicitly[RefType[refined.api.Refined]].unsafeWrap(ne.value)

  sealed trait Refined[+T, P] extends Any {
    private[nonempty] def value: T
  }
  object Refined {
    final class Impl[+T, P] private[nonempty] (val value: T)
        extends Refined[T, P]

    def unsafeApply[T, P](t: T): Refined[T, P] = new Impl(t)
  }

  implicit val refinedRefType: RefType[Refined] =
    new RefType[Refined] {
      override def unsafeWrap[T, P](t: T): Refined[T, P] =
        Refined.unsafeApply(t)

      override def unwrap[T](tp: Refined[T, _]): T =
        tp.value

      override def unsafeRewrap[T, A, B](ta: Refined[T, A]): Refined[T, B] =
        Refined.unsafeApply(ta.value)

      override def unsafeWrapM[T: c.WeakTypeTag, P: c.WeakTypeTag](
          c: blackbox.Context
      )(t: c.Expr[T]): c.Expr[Refined[T, P]] =
        c.universe.reify(Refined.unsafeApply(t.splice))

      override def unsafeRewrapM[T: c.WeakTypeTag, A: c.WeakTypeTag, B: c.WeakTypeTag](
          c: blackbox.Context
      )(ta: c.Expr[Refined[T, A]]): c.Expr[Refined[T, B]] =
        c.universe.reify(Refined.unsafeApply(ta.splice.value))
    }
}
