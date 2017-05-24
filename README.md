nonempty [![Build Status][travis-img]][travis] [![Maven Central][maven-img]][maven] [![Scaladoc][javadoc-img]][javadoc]
========

A type to statically guaranteeing or requring nonemptiness of lists.

Getting started
---------------

Add dependency in your `build.sbt` as the following.

```scala
    libraryDependencies ++= Seq(
      ...
      "com.github.tarao" %% "nonempty" % "0.0.7"
    )
```

The library is available on [Maven Central][maven].  Currently,
supported Scala version is 2.11 and 2.12.

Use case
--------

### Guaranteeing nonemptiness

Type `NonEmpty[A]` denotes that it is a nonempty list whose elements
are of type `A`.  There are only two ways to instantiate a
`NonEmpty[A]`:

- calling `NonEmpty.apply`, which takes at least one argument, or
- calling `NonEmpty.fromIterable`, which returns `Option[NonEmpty[_]]`.

For example, calling `NonEmpty.apply` with one or two arguments is
legal but calling it with zero arguments is illegal.

```scala
val one = NonEmpty(1)
val two = NonEmpty(1, 2)
val zero = NonEmpty() // Compile error
```

`NonEmpty.fromIterable` takes an `Iterable[_]` with an arbitrary
number of elements.  In this case, the return type is
`Option[NonEmpty[_]]` and its value is `None` if you passed an empty
`Iterable[]`.

```scala
val s: Seq[Int] = ...
val maybeEmpty: Option[NonEmpty[Int]] = NonEmpty.fromIterable(s)

val Some(nonempty) = NonEmpty.fromIterable(Seq(1, 2, 3))

val None = NonEmpty.fromIterable(Seq())
```

If you pass a mutable collection to `NonEmpty.fromIterable` then the
elements of the collection are copied.  Otherwise, `NonEmpty` holds
the original (immutable) collection.

### Requring nonempty lists

When you require an argument to be nonempty, then you just have to
define a method which takes a `NonEmpty[]`.

```scala
def requiresNonEmpty(nonempty: NonEmpty[Int]) = ...
```

Then it is the caller of the method who is obliged to pass a nonempty
list.  The caller may need a pattern matching if there is no guarantee
that the list isn't empty.

```scala
requiresNonEmpty(NonEmpty(1, 2, 3))

val s: Seq[Int] = ...
val maybeEmpty = NonEmpty.fromIterable(s)
maybeEmpty match {
  case Some(nonempty) => requiresNonEmpty(nonempty)
  case _              => ... /* do something else */
}
```

If you allow an empty list by providing a default value, then you can
define a method which takes an `Option[NonEmpty[_]]`.  In this case,
you need a pattern matching inside the method.

```scala
def acceptsEmpty(maybeEmpty: Option[NonEmpty[Int]]) =
  maybeEmpty match {
    case Some(nonempty) => requiresNonEmpty(nonempty)
    case None           => ... /* provide a default value */
  }

val s: Seq[Int] = ...
acceptsEmpty(s)
```

Note that it is possible to pass an `Iterable[A]` as an argument of
type `Option[NonEmpty[A]]` because there is an implicit conversion
from `Iterable[A]` to `Option[NonEmpty[A]]` for any `A`.

### Preserving nonemptiness

Some collection methods such as `map()` preserve nonemptiness.  The
methods are [those which are directly defined in `class NonEmpty[]`](http://javadoc-badge.appspot.com/com.github.tarao/nonempty_2.12/com/github/tarao/nonempty/NonEmpty.html).

```scala
val nonempty: NonEmpty[Int] = NonEmpty(1, 2, 3).map(x => x * x)
```

Note that if you wish to use `scala.collection.breakOut` for these
methods, you actually need to use `com.github.tarao.nonempty.breakOut`
instead.

```scala
import com.github.tarao.nonempty.breakOut
val m: Map[Int, Int] = NonEmpty(1, 2, 3).map(x => x -> x * x)(breakOut)
```

### Breaking nonemptiness

You can freely convert a `NonEmpty[_]` into an `Iterable[_]`.

```scala
val list: Iterable[Int] = NonEmpty(1, 2, 3)
```

You can also call any collection method which does not preserve
nonemptiness.  If the method returns a collection then the type of the
collection is `Iterable[_]`.

```scala
val nonempty: NonEmpty[Int] = ...
val list: Iterable[Int] = nonempty.filter(_ % 2 == 0)
```

These methods are called via an implicit conversion from `NonEmpty[A]`
to `Iterable[A]`.

License
-------

- Copyright (C) INA Lintaro
- MIT License

[travis]: https://travis-ci.org/tarao/nonempty-scala
[travis-img]: https://img.shields.io/travis/tarao/nonempty-scala.svg?branch=master&style=flat
[maven]: https://maven-badges.herokuapp.com/maven-central/com.github.tarao/nonempty_2.12
[maven-img]: https://maven-badges.herokuapp.com/maven-central/com.github.tarao/nonempty_2.12/badge.svg?style=flat
[javadoc]: http://javadoc-badge.appspot.com/com.github.tarao/nonempty_2.12
[javadoc-img]: http://javadoc-badge.appspot.com/com.github.tarao/nonempty_2.12.svg?label=scaladoc
