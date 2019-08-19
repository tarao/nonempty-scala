nonempty [![Build Status][travis-img]][travis] [![Maven Central][maven-img]][maven] [![Scaladoc][javadoc-img]][javadoc]
========

A type to statically guaranteeing or requring non-emptiness of lists.

Getting started
---------------

Add dependency in your `build.sbt` as the following.

For Scala 2.11 and 2.12 :

```scala
    libraryDependencies ++= Seq(
      ...
      "com.github.tarao" %% "nonempty" % "0.0.8"
    )
```

For Scala 2.13 :

```scala
    libraryDependencies ++= Seq(
      ...
      "com.github.tarao" %% "nonempty" % "0.2.0"
    )
```

The library is available on [Maven Central][maven].

Use case
--------

### Guaranteeing non-emptiness

Type `NonEmpty[A]` denotes that it is a non-empty list whose elements
are of type `A`.  There are only two ways to instantiate a
`NonEmpty[A]`:

- calling `NonEmpty.apply`, which takes at least one argument, or
- calling `NonEmpty.fromIterable`, which returns `Option[NonEmpty[_]]`.

For example, calling `NonEmpty.apply` with one or two arguments is
legal but calling it with zero arguments is illegal.

```scala
import com.github.tarao.nonempty.NonEmpty

val one = NonEmpty(1)
val two = NonEmpty(1, 2)
val zero = NonEmpty() // Compile error
```

`NonEmpty.fromIterable` takes an `Iterable[_]` with an arbitrary
number of elements.  In this case, the return type is
`Option[NonEmpty[_]]` and its value is `None` if you passed an empty
`Iterable[_]`.

```scala
val s: Seq[Int] = ...
val maybeEmpty: Option[NonEmpty[Int]] = NonEmpty.fromIterable(s)

val Some(nonempty) = NonEmpty.fromIterable(Seq(1, 2, 3))

val None = NonEmpty.fromIterable(Seq())
```

If you pass a mutable collection to `NonEmpty.fromIterable` then the
elements of the collection are copied.  Otherwise, `NonEmpty` holds
the original (immutable) collection.

### Requring non-empty lists

When you require an argument to be non-empty, then you just have to
define a method which takes a `NonEmpty[_]`.

```scala
def requiresNonEmpty(ne: NonEmpty[Int]) = ...
```

Then it is the caller of the method who is obliged to pass a non-empty
list.  The caller may need a pattern matching if there is no guarantee
that the list isn't empty.

```scala
requiresNonEmpty(NonEmpty(1, 2, 3))

val s: Seq[Int] = ...
val maybeEmpty = NonEmpty.fromIterable(s)
maybeEmpty match {
  case Some(ne) => requiresNonEmpty(ne)
  case _        => ... /* do something else */
}
```

If you allow an empty list by providing a default value, then you can
define a method which takes an `Option[NonEmpty[_]]`.  In this case,
you need a pattern matching inside the method.

```scala
def acceptsEmpty(maybeEmpty: Option[NonEmpty[Int]]) =
  maybeEmpty match {
    case Some(ne) => requiresNonEmpty(ne)
    case None     => ... /* provide a default value */
  }

val s: Seq[Int] = ...
acceptsEmpty(s)
```

Note that it is possible to pass an `Iterable[A]` as an argument of
type `Option[NonEmpty[A]]` because there is an implicit conversion
from `Iterable[A]` to `Option[NonEmpty[A]]` for any `A`.

### Preserving nonemptiness

Some collection methods such as `map()` preserve non-emptiness.  The
methods are [those which are directly defined in `class NonEmpty[_]`](http://javadoc-badge.appspot.com/com.github.tarao/nonempty_2.13/com/github/tarao/nonempty/collection/NonEmpty.html).

```scala
val ne: NonEmpty[Int] = NonEmpty(1, 2, 3).map(x => x * x)
```

### Breaking non-emptiness

You can freely convert a `NonEmpty[_]` into an `Iterable[_]`.

```scala
val list: Iterable[Int] = NonEmpty(1, 2, 3)
```

You can also call any collection method which does not preserve
non-emptiness.  If the method returns a collection then the type of
the collection is `Iterable[_]`.

```scala
val ne: NonEmpty[Int] = ...
val list: Iterable[Int] = ne.filter(_ % 2 == 0)
```

These methods are called via an implicit conversion from `NonEmpty[A]`
to `Iterable[A]`.

### Specific non-empty collection types (requires >= 0.2.0)

Above examples describe how you can give non-emptiness to an
`Iterable[A]`.  You can also make an arbitrary `C <: Iterable[A]`
non-empty.  There is a type `collection.NonEmpty[A, C]` for this.

```scala
import com.github.tarao.nonempty.collection.NonEmpty

// implicit
val maybeNonEmpty: Option[NonEmpty[Int, Vector[Int]]] = Vector(1, 2, 3)
// => maybeNonEmpty: Option[NonEmpty[Int, Vector[Int]]] = Some(Vector(1, 2, 3))

// explicit
val Some(ne) = NonEmpty.from(Vector(1, 2, 3))
// => ne: NonEmpty[Int, Vector[Int]] = Vector( 1, 2, 3)
```

Note that these new interface only works with immutable collections.
Passing a mutable collection is statically accepted but result in
`None`.  You should make it immutable before marking it as non-empty.

```scala
val maybeEmpty = NonEmpty.from(mutable.ArrayBuffer(1, 2, 3))
// => maybeEmpty: Option[NonEmpty[Int, mutable.ArrayBuffer[Int]]] = None

val maybeEmpty = NonEmpty.from(mutable.ArrayBuffer(1, 2, 3).toIndexedSeq)
// => maybeEmpty: Option[NonEmpty[Int, IndexedSeq[Int]]] = Some(Vector(1, 2, 3))
```

There is also a direct constructor for specific non-empty collections.
The returned collection value is always immutable.

```scala
val nel = NonEmpty[List[Int]](1, 2, 3)
// => nel: NonEmpty[Int, List[Int]] = List(1, 2, 3)

val nes = NonEmpty[Set[String]]("foo", "bar")
// => nes: NonEmpty[String, Set[String]] = Set(foo, bar)
```

License
-------

- Copyright (C) INA Lintaro
- MIT License

[travis]: https://travis-ci.org/tarao/nonempty-scala
[travis-img]: https://img.shields.io/travis/tarao/nonempty-scala.svg?branch=master&style=flat
[maven]: https://maven-badges.herokuapp.com/maven-central/com.github.tarao/nonempty_2.13
[maven-img]: https://maven-badges.herokuapp.com/maven-central/com.github.tarao/nonempty_2.13/badge.svg?style=flat
[javadoc]: http://javadoc-badge.appspot.com/com.github.tarao/nonempty_2.13
[javadoc-img]: http://javadoc-badge.appspot.com/com.github.tarao/nonempty_2.13.svg?label=scaladoc
