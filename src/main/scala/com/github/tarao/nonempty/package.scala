package com.github.tarao

package object nonempty {
  /** An alias of `NonEmpty[A, Iterable[A]]`.
    *
    * @see [[collection.NonEmpty]]
    */
  type NonEmpty[+A] = collection.NonEmpty[A, Iterable[A]]
  object NonEmpty {
    /** Create a `NonEmpty` instance by providing at least one element.
      *
      * Example {{{
      *   val ne = NonEmpty(1, 2, 3)
      * }}}
      *
      * @tparam C the collection type
      * @return a non-empty collection.
      */
    def apply[A](head: A, elements: A*): NonEmpty[A] =
      collection.NonEmpty[Iterable[A]](head, elements: _*)

    /** Convert a collection to `Option[NonEmpty[A]]`.
      *
      * Note: There is no way to directly convert a collection into a
      *       `NonEmpty[_]`.
      *
      * Example {{{
      *   val ne = NonEmpty.fromIterable(List(1, 2, 3))
      * }}}
      *
      * @return `Some` refined collection value if it is neither empty
      *          nor mutable, or otherwise `None`.
      */
    def fromIterable[A](it: Iterable[A]): Option[NonEmpty[A]] =
      collection.NonEmpty.from(it)
  }
}
