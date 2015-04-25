nonempty [![Build Status][travis-img]][travis]
========

A trait to restrict a collection to be non-empty.

Use case
--------

Use `NonEmpty[T]` instead of `Traversable[T]` as a parameter type of a
method where you expect a non-empty collection.

```scala
import com.github.tarao.nonempty.NonEmpty

def requireNonEmpty[T](nonEmpty: NonEmpty[T]) = ???
```

To pass an argument to the method, one must make an instance of
`NonEmpty[T]` via a singleton object `NonEmpty`.

```scala
requireNonEmpty(NonEmpty(1, 2, 3)) // create a NonEmpty[Int] directly

// create a NonEmpty[Int] from a Traversable[Int]
val list: Option[NonEmpty[Int]] = NonEmpty.fromTraversable(Seq(1, 2, 3))
list match {
  case Some(ne) => requireNonEmpty(ne)
  case None => sys.error("unexpected")
}
```

Note that `NonEmpty.fromTraversable` takes an arbitrary
`Traversable[T]`, but it only returns an `Option[NonEmpty[T]]`, whcih
will be `Some(_)` if the collection is actually not empty or `None` if
it is empty.  The only way to create an instance of `NonEmpty[T]`
directly is to call `NonEmpty.apply`, which takes at least one
argument.

The latter form `NonEmpty.fromTraversable` can be omitted since it is
an implicit conversion.

```scala
val list: Option[NonEmpty[Int]] = Seq(1, 2, 3)
```

This means that you can pass an arbitrary `Traversable[T]` to a method
which takes an `Option[NonEmpty[T]]`.  (You have to provide a default
behavior in the method if it appears to be `None`, of course.)

```scala
def requireMaybeNonEmpty[T](maybeNonEmpty: Option[NonEmpty[T]]) = ???
```

```scala
requireMaybeNonEmpty(Seq(1, 2, 3)) // OK
```

In a method which takes a `NonEmpty[T]`, the value can be treated as a
`Traversable[T]` since an implicit conversion from `NonEmpty[T]` to
`Traversable[T]` is provided.

License
-------

- Copyright (C) INA Lintaro
- MIT License

[travis]: https://travis-ci.org/tarao/nonempty-scala
[travis-img]: https://img.shields.io/travis/tarao/nonempty-scala.svg?branch=master&style=flat
