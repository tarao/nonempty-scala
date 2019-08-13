package com.github.tarao.nonempty.collection

import scala.collection.immutable

/** Methods inherited from `Queue` that preserve non-emptiness. */
trait QueueOps[+A, +C <: Iterable[A]] extends Any {
  self: NonEmpty[A, C] =>

  /** Creates a new queue with element added at the end
    * of the old queue.
    *
    * @param  elem        the element to insert
    * @see [[scala.collection.immutable.Queue!.enqueue[B>:A](elem:B)*]]
    */
  def enqueue[B >: A](elem: B)(implicit
    coll: C => immutable.Queue[B]
  ): NonEmpty[B, immutable.Queue[B]] =
    unsafeApply[B, immutable.Queue[B]](coll(value).enqueue(elem))

  /** Creates a new queue with all elements provided by an `Iterable`
    * object added at the end of the old queue.
    *
    * The elements are appended in the order they are given out by the
    * iterator.
    *
    * @param  iter        an iterable object
    * @see [[scala.collection.immutable.Queue!.enqueueAll]]
    */
  def enqueueAll[B >: A](iter: Iterable[B])(implicit
    coll: C => immutable.Queue[B]
  ): NonEmpty[B, immutable.Queue[B]] =
    unsafeApply[B, immutable.Queue[B]](coll(value).enqueueAll(iter))
}
