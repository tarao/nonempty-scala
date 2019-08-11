package com.github.tarao.nonempty.collection

import scala.annotation.implicitNotFound
import scala.collection.{BuildFrom, Map => ColMap, Set => ColSet}
import scala.collection.immutable
import scala.language.higherKinds

/** Methods inherited from `Map` that preserve non-emptiness. */
trait MapOps[+A, +C <: Iterable[A]] extends Any {
  self: NonEmpty[A, C] =>

  /** Collects all keys of this map in a set.
    * @return  a set containing all keys of this map.
    * @see [[scala.collection.MapOps!.keySet]]
    */
  def keySet[K, V, M <: ColMap[K, V], S <: ColSet[K]](implicit
    m: MapOps.IsMap[C, K, V, M, S]
  ): NonEmpty[K, S] = unsafeApply[K, S](m.keySet(value))

  /** Collects all keys of this map in an iterable collection.
    *
    * @return the keys of this map as an iterable.
    * @see [[scala.collection.MapOps!.keys]]
    */
  def keys[K, V](implicit
    coll: C => ColMap[K, V]
  ): NonEmpty[K, Iterable[K]] = unsafeApply[K, Iterable[K]](coll(value).keys)

  /** Collects all values of this map in an iterable collection.
    *
    * @return the values of this map as an iterable.
    * @see [[scala.collection.MapOps!.values]]
    */
  def values[K, V](implicit
    coll: C => ColMap[K, V]
  ): NonEmpty[V, Iterable[V]] = unsafeApply[V, Iterable[V]](coll(value).values)

  /** Creates a new map obtained by updating this map with a given
    * key/value pair.
    * @param    key the key
    * @param    value the value
    * @tparam   V1 the type of the added value
    * @return   A new map with the new key/value mapping added to this map.
    * @see [[scala.collection.immutable.MapOps!.updated]]
    */
  def updated[K, V, C2 <: Iterable[(K, V)]](key: K, value: V)(implicit
    coll: C => immutable.Map[K, V],
    bf: BuildFrom[C, (K, V), C2],
  ): NonEmpty[(K, V), C2] = unsafeApply[(K, V), C2](
    bf.fromSpecific(self.value)(coll(self.value).updated(key, value))
  )

  /** Alias for `updated`
    *
    * @param kv the key/value pair.
    * @tparam V1 the type of the value in the key/value pair.
    * @return A new map with the new binding added to this map.
    * @see [[#updated]]
    */
  def +[K, V, C2 <: Iterable[(K, V)]](kv: (K, V))(implicit
    coll: C => immutable.Map[K, V],
    bf: BuildFrom[C, (K, V), C2],
  ): NonEmpty[(K, V), C2] = updated(kv._1, kv._2)

  // `transform[W](f: (K, V) => W)(implicit coll: C => Map[K, V])` is
  // not placed here.  It is defined in `MapOps.Implicits.MapOps` as
  // an extension method.  It cannot be implemented as a direct method
  // here or otherwise a caller cannot infer the parameter type `(K,
  // V)` of `f`.
}
object MapOps {
  @implicitNotFound(msg = "${C} is neither scala.collection.Map nor scala.collection.immutable.Map")
  abstract class IsMap[-C, K, V, M <: ColMap[K, V], S <: ColSet[K]] {
    def keySet(c: C): S
  }
  object IsMap extends IsMapLowPriority {
    implicit def isImmutableMap[C, K, V](implicit
      ev: C => immutable.Map[K, V]
    ): IsMap[C, K, V, immutable.Map[K, V], immutable.Set[K]] =
      new IsMap[C, K, V, immutable.Map[K, V], immutable.Set[K]] {
        def keySet(c: C): immutable.Set[K] = ev(c).keySet
      }
  }
  trait IsMapLowPriority {
    implicit def isMap[C, K, V](implicit
      ev: C => ColMap[K, V]
    ): IsMap[C, K, V, ColMap[K, V], ColSet[K]] =
      new IsMap[C, K, V, ColMap[K, V], ColSet[K]] {
        def keySet(c: C): ColSet[K] = ev(c).keySet
      }
  }

  trait Implicits {
    protected def unsafeApply[A, C <: Iterable[A]](it: C) : NonEmpty[A, C]

    implicit class MapOps[K, V, CC[X, Y] <: Map[X, Y]](
      private val ne: NonEmpty[(K, V), CC[K, V]]
    )  {
      /** This function transforms all the values of mappings contained in
        * this map with function `f`.
        *
        * @param f A function over keys and values
        * @return  the updated map
        * @see [[scala.collection.immutable.MapOps!.transform]]
        */
      // This placed here (not in `trait MapOps`) because otherwise we
      // cannot let parameter types `(K, V)` of `f` be inferred.
      def transform[W](f: (K, V) => W)(implicit
        bf: BuildFrom[CC[K, V], (K, W), CC[K, W]]
      ): NonEmpty[(K, W), CC[K, W]] = unsafeApply[(K, W), CC[K, W]](
        bf.fromSpecific(ne.value)(ne.value.transform(f))
      )
    }
  }
}
