package com.github.tarao.nonempty.collection

/** Methods inherited from `List` that preserve non-emptiness. */
trait ListOps[+A, +C <: Iterable[A]] extends Any {
  self: NonEmpty[A, C] =>

  /** Adds an element at the beginning of this list.
    * @param elem the element to prepend.
    * @return  a list which contains `x` as first element and
    *          which continues with this list.
    * @see [[scala.collection.immutable.List!.::]]
    */
  def ::[B >: A](elem: B)(implicit coll: C => List[B]): NonEmpty[B, List[B]] =
    unsafeApply[B, List[B]](elem :: coll(value))

  /** Adds the elements of a given list in front of this list.
    *
    * @param prefix  The list elements to prepend.
    * @return a list resulting from the concatenation of the given
    *         list `prefix` and this list.
    * @see [[scala.collection.immutable.List!.:::]]
    */
  def :::[B >: A](prefix: List[B])(implicit
    coll: C => List[B]
  ): NonEmpty[B, List[B]] = unsafeApply[B, List[B]](prefix ::: coll(value))

  /** Adds the elements of a given list in reverse order in front of this list.
    * `xs reverse_::: ys` is equivalent to
    * `xs.reverse ::: ys` but is more efficient.
    *
    * @param prefix the prefix to reverse and then prepend
    * @return       the concatenation of the reversed prefix and the current list.
    * @see [[scala.collection.immutable.List!.reverse_:::]]
    */
  def reverse_:::[B >: A](prefix: List[B])(implicit
    coll: C => List[B]
  ): NonEmpty[B, List[B]] =
    unsafeApply[B, List[B]](coll(value).reverse_:::(prefix))

  /** Builds a new list by applying a function to all elements of this
    * list.  Like `xs map f`, but returns `xs` unchanged if function
    * `f` maps all elements to themselves (as determined by `eq`).
    *
    * @param f      the function to apply to each element.
    * @tparam B     the element type of the returned collection.
    * @return       a list resulting from applying the given function
    *               `f` to each element of this list and collecting the results.
    * @see [[scala.collection.immutable.List!.mapConserve]]
    */
  @inline def mapConserve[A1 >: A, B >: A <: AnyRef](f: A => B)(implicit
    coll: C => List[A1]
  ): NonEmpty[B, List[B]] =
    unsafeApply[B, List[B]](value.toList.mapConserve(f))
}
