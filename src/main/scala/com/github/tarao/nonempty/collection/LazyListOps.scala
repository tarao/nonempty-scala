package com.github.tarao.nonempty.collection

/** Methods inherited from `LazyList` that preserve non-emptiness.
  *
  *  @define appendStackSafety Note: Repeated chaining of calls to
  *                            append methods (`appended`,
  *                            `appendedAll`, `lazyAppendedAll`)
  *                            without forcing any of the intermediate
  *                            resulting lazy lists may overflow the
  *                            stack when the final result is forced.
  *  @define preservesLaziness This method preserves laziness;
  *                            elements are only evaluated
  *                            individually as needed.
 */
trait LazyListOps[+A, +C <: Iterable[A]] extends Any {
  self: NonEmpty[A, C] =>

  /** Evaluates all undefined elements of the lazy list.
    *
    * This method detects cycles in lazy lists, and terminates after all
    * elements of the cycle are evaluated.
    *
    * This method will *not* terminate for non-cyclic infinite-sized
    * collections.
    *
    * @return this
    * @see [[scala.collection.immutable.LazyList!.force]]
    */
  def force[B >: A](implicit
    coll: C => LazyList[B],
  ): this.type = {
    coll(value).force
    this
  }

  /** The lazy list resulting from the concatenation of this lazy list
    * with the argument lazy list.
    *
    * $preservesLaziness
    *
    * $appendStackSafety
    *
    * @param suffix The collection that gets appended to this lazy list
    * @return The lazy list containing elements of this lazy list and the iterable object.
    * @see [[scala.collection.immutable.LazyList!.lazyAppendedAll]]
    */
  def lazyAppendedAll[B >: A](suffix: => IterableOnce[B])(implicit
    coll: C => LazyList[B]
  ): NonEmpty[B, LazyList[B]] =
    unsafeApply[B, LazyList[B]](coll(value).lazyAppendedAll(suffix))
}
object LazyListOps {
  trait Implicits {
    protected def unsafeApply[A, C <: Iterable[A]](it: C) : NonEmpty[A, C]

    implicit class Deferrer[A](private val nel: NonEmpty[A, LazyList[A]]) {
      /** Construct a LazyList consisting of a given first element followed
        * by elements from another LazyList.
        */
      def #::[B >: A](elem: => B): NonEmpty[B, LazyList[B]] =
        unsafeApply[B, LazyList[B]](elem #:: nel.value)

      /** Construct a LazyList consisting of the concatenation of the given
        * LazyList and another LazyList.
        */
      def #:::[B >: A](prefix: LazyList[B]): NonEmpty[B, LazyList[B]] =
        unsafeApply[B, LazyList[B]](prefix #::: nel.value)
    }
  }
}
