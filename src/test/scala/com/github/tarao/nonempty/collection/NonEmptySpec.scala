package com.github.tarao.nonempty.collection

import eu.timepit.refined
import eu.timepit.refined.refineV
import eu.timepit.refined.api.{Refined, RefType}
import org.scalatest.{FunSpec, Inside, Inspectors, Matchers, OptionValues}
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

class NonEmptySpec extends FunSpec
    with Matchers with OptionValues with Inside with Inspectors {
  def typeEquals[T1, T2](a: T1, b: T2)(implicit ev: T1 =:= T2): Boolean = true

  object AsParameterOf {
    def optionNonEmpty[A, C <: Iterable[A]](nel: Option[NonEmpty[A, C]]): Unit = {}
    def nonEmpty[A, C <: Iterable[A]](nel: NonEmpty[A, C]): Unit = {}
    def iterable[A](it: Iterable[A]): Unit = {}
  }

  trait MaybeEmpty[T]
  object MaybeEmpty {
    implicit def valid[T]: MaybeEmpty[T] = new MaybeEmpty[T] {}
    implicit def ambig1[A, C <: Iterable[A]]: MaybeEmpty[NonEmpty[A, C]] =
      sys.error("unexpected")
    implicit def ambig2[A, C <: Iterable[A]]: MaybeEmpty[NonEmpty[A, C]] =
      sys.error("unexpected")
  }

  def maybeEmpty[T](x: T)(implicit ev: MaybeEmpty[T]): Boolean = true

  describe("Instantiation") {
    it("should provide apply[Coll](elem, ...) interface") {
      val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
      val nel2 = NonEmpty[Vector[_]](1, 2, 3)
      val nel3: NonEmpty[Int, Vector[Int]] = nel1
      val nel4: NonEmpty[Int, Vector[Int]] = nel2
      typeEquals(nel1, nel2) shouldBe true
      typeEquals(nel2, nel3) shouldBe true
      typeEquals(nel3, nel4) shouldBe true
    }

    it("cannot be constructed from an empty parameter list") {
      assertTypeError("val ne = NonEmpty[Vector[Int]]()")
    }
  }

  describe("A context of type Option[NonEmpty[_, _]]") {
    it("should accept a Iterable[_]") {
      // as a varibable
      val nel1: Option[NonEmpty[Int, Seq[Int]]] = Seq(1, 2, 3)
      val nel2: Option[NonEmpty[Int, Seq[Int]]] = Seq.empty[Int]
      val nel3: Option[NonEmpty[Int, ArrayBuffer[Int]]] = ArrayBuffer(1, 2, 3)
      val nel4: Option[NonEmpty[Int, ArrayBuffer[Int]]] = ArrayBuffer.empty[Int]
      val nel5: Option[NonEmpty[Int, Iterable[Int]]] = Seq(1, 2, 3)
      val nel6: Option[NonEmpty[Int, mutable.Iterable[Int]]] = ArrayBuffer(1, 2, 3)
      val nel7: Option[NonEmpty[(Int, String), Map[Int, String]]] = Map(1 -> "foo")
      val nel8: Option[NonEmpty[Int, immutable.BitSet]] = immutable.BitSet(1, 2, 3)
      val nel9: Option[NonEmpty[Int, mutable.BitSet]] = mutable.BitSet(1, 2, 3)
      val nel10: Option[NonEmpty[Char, immutable.WrappedString]] = "foo"

      // as a parameter
      AsParameterOf.optionNonEmpty(Seq(1, 2, 3))
      AsParameterOf.optionNonEmpty(Seq.empty[Int])
      AsParameterOf.optionNonEmpty(ArrayBuffer(1, 2, 3))
      AsParameterOf.optionNonEmpty(ArrayBuffer.empty[Int])
      AsParameterOf.optionNonEmpty[Int, Iterable[Int]](Seq(1, 2, 3))
      AsParameterOf.optionNonEmpty[Int, mutable.Iterable[Int]](ArrayBuffer(1, 2, 3))
      AsParameterOf.optionNonEmpty[(Int, String), Map[Int, String]](Map(1 -> "foo"))
      AsParameterOf.optionNonEmpty[Int, immutable.BitSet](immutable.BitSet(1, 2, 3))
      AsParameterOf.optionNonEmpty[Int, mutable.BitSet](mutable.BitSet(1, 2, 3))
      AsParameterOf.optionNonEmpty[Char, immutable.WrappedString]("foo")
    }

    it("should reject mutable collections") {
      locally {
        val it = mutable.Stack(1, 2, 3)
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.Queue(1, 2, 3)
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.PriorityQueue(1, 2, 3)
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.ArraySeq(1, 2, 3)
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.ArrayBuffer(1, 2, 3)
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.ListBuffer(1, 2, 3)
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.HashSet(1, 2, 3)
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.LinkedHashSet(1, 2, 3)
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.TreeSet(1, 2, 3)
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.BitSet(1, 2, 3)
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.HashMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.LinkedHashMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val None = NonEmpty.from(it)
      }

      locally {
        val it = mutable.TreeMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val None = NonEmpty.from(it)
      }
    }
  }

  describe("A context of type NonEmpty[_, _]") {
    it("should not accept a Iterable[_]") {
      // as a varibable
      assertTypeError("val ne1: NonEmpty[Int, Seq[Int]] = Seq(1, 2, 3)")
      assertTypeError("val ne2: NonEmpty[Int, Seq[Int]] = Seq.empty[Int]")
      assertTypeError("val ne3: NonEmpty[Int, ArrayBuffer[Int]] = ArrayBuffer(1, 2, 3)")
      assertTypeError("val ne4: NonEmpty[Int, ArrayBuffer[Int]] = ArrayBuffer.empty[Int]")

      // as a parameter
      assertTypeError("nonEmpty[Int, Seq[Int]](Seq(1, 2, 3))")
      assertTypeError("nonEmpty[Int, Seq[Int]](Seq.empty[Int])")
      assertTypeError("nonEmpty[Int, ArrayBuffer[Int]](ArrayBuffer(1, 2, 3))")
      assertTypeError("nonEmpty[Int, ArrayBuffer[Int]](ArrayBuffer.empty[Int])")
    }
  }

  describe("A value of NonEmpty[_, _]") {
    it("should behave like a Iterable[]") {
      val nel = NonEmpty[Vector[Int]](1, 2, 3)
      AsParameterOf.iterable(nel)

      val it: Iterable[Int] = nel
      val vec: Vector[Int] = nel

      // method
      val ss = nel.flatMap(n => Seq.fill(n)(n))
      ss shouldBe Seq(1, 2, 2, 3, 3, 3)
      val s = nel.mkString
      s shouldBe "123"
      val n = nel.foldLeft(0)(_ + _)
      n shouldBe 6
    }

    it("should be compatible with broader collection type") {
      val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
      val nel2: NonEmpty[Int, Iterable[Int]] = nel1
    }

    it("should be identical to its immutable Iterable[_] counterpart") {
      locally {
        val it1 = List(1)
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = List(1, 2, 3)
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: List[Int] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[Int, List[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = LazyList(1)
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = LazyList(1, 2, 3)
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: LazyList[Int] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[Int, LazyList[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.Queue(1)
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.Queue(1, 2, 3)
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: immutable.Queue[Int] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[Int, immutable.Queue[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = Vector(1)
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = Vector(1, 2, 3)
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: Vector[Int] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[Int, immutable.Vector[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = Set(1)
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = Set(1, 2, 3)
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: Set[Int] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[Int, immutable.Set[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.HashSet(1)
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.HashSet(1, 2, 3)
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: immutable.HashSet[Int] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[Int, immutable.HashSet[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.SortedSet(1)
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.SortedSet(1, 2, 3)
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: immutable.SortedSet[Int] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[Int, immutable.SortedSet[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.ListSet(1)
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.ListSet(1, 2, 3)
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: immutable.ListSet[Int] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[Int, immutable.ListSet[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.TreeSet(1)
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.TreeSet(1, 2, 3)
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: immutable.TreeSet[Int] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[Int, immutable.TreeSet[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.BitSet(1)
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.BitSet(1, 2, 3)
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: immutable.BitSet = nel1
        nel1.value should be theSameInstanceAs it3

        typeEquals(nel1, nel2) shouldBe true
      }

      locally {
        val it1 = Map(1 -> "foo")
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = Map(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: Map[Int, String] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[(Int, String), Map[Int, String]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.HashMap(1 -> "foo")
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.HashMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: immutable.HashMap[Int, String] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[(Int, String), immutable.HashMap[Int, String]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.SortedMap(1 -> "foo")
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.SortedMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: immutable.SortedMap[Int, String] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[(Int, String), immutable.SortedMap[Int, String]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.ListMap(1 -> "foo")
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.ListMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: immutable.ListMap[Int, String] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[(Int, String), immutable.ListMap[Int, String]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val it1 = immutable.TreeMap(1 -> "foo")
        val Some(nel1) = NonEmpty.from(it1)
        nel1.value should be theSameInstanceAs it1

        val it2 = immutable.TreeMap(1 -> "foo", 2 -> "bar", 3 -> "baz")
        val Some(nel2) = NonEmpty.from(it2)
        nel2.value should be theSameInstanceAs it2

        val it3: immutable.TreeMap[Int, String] = nel1
        nel1.value should be theSameInstanceAs it3

        val nel3: NonEmpty[(Int, String), immutable.TreeMap[Int, String]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
      }

      locally {
        val s1 = "foo"
        val Some(nel1) = NonEmpty.from(s1)
        nel1.value.unwrap should be theSameInstanceAs s1

        val it2: String = nel1
        nel1.value.unwrap should be theSameInstanceAs it2

        val nel2: NonEmpty[Char, immutable.WrappedString] = nel1
        typeEquals(nel1, nel2) shouldBe true
      }
    }
  }

  describe("Preserving non-emptiness") {
    describe("IterableOps") {
      it("should preserve non-emptyness after .to()") {
        locally {
          val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
          val nel2 = nel1.to(List)
          val nel3: NonEmpty[Int, List[Int]] = nel2
          typeEquals(nel2, nel3)
          nel2.value shouldBe List(1, 2, 3)
        }

        locally {
          val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
          val nel2 = nel1.to(immutable.BitSet)
          val nel3: NonEmpty[Int, immutable.BitSet] = nel2
          typeEquals(nel2, nel3)
          nel2.value shouldBe immutable.BitSet(1, 2, 3)
        }

        locally {
          val nel1 = NonEmpty[Vector[(Int, String)]](1 -> "foo", 2 -> "bar")
          val nel2 = nel1.to(Map)
          val nel3: NonEmpty[(Int, String), Map[Int, String]] = nel2
          typeEquals(nel2, nel3)
          nel2.value shouldBe Map(1 -> "foo", 2 -> "bar")
        }

        locally {
          val nel1 = NonEmpty[Vector[Char]]('f', 'o', 'o')
          val nel2 = nel1.to(immutable.WrappedString)
          val nel3: NonEmpty[Char, immutable.WrappedString] = nel2
          typeEquals(nel2, nel3)
          nel2.value.unwrap shouldBe "foo"
        }
      }

      it("should preserve non-emptyness after .toList") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1.toList
        val nel3: NonEmpty[Int, List[Int]] = nel2
        typeEquals(nel2, nel3)
        nel2.value shouldBe List(1, 2, 3)
      }

      it("should preserve non-emptyness after .toVector") {
        val nel1 = NonEmpty[List[Int]](1, 2, 3)
        val nel2 = nel1.toVector
        val nel3: NonEmpty[Int, Vector[Int]] = nel2
        typeEquals(nel2, nel3)
        nel2.value shouldBe Vector(1, 2, 3)
      }

      it("should preserve non-emptyness after .toMap") {
        val nel1 = NonEmpty[Vector[(Int, String)]](1 -> "foo", 2 -> "bar")
        val nel2 = nel1.toMap
        val nel3: NonEmpty[(Int, String), Map[Int, String]] = nel2
        typeEquals(nel2, nel3)
        nel2.value shouldBe Map(1 -> "foo", 2 -> "bar")
      }

      it("should preserve non-emptyness after .toSeq") {
        val nel1 = NonEmpty[Set[Int]](1, 2, 3)
        val nel2 = nel1.toSeq
        val nel3: NonEmpty[Int, Seq[Int]] = nel2
        typeEquals(nel2, nel3)
        nel2.value shouldBe Seq(1, 2, 3)
      }

      it("should preserve non-emptyness after .toIndexedSeq") {
        val nel1 = NonEmpty[Set[Int]](1, 2, 3)
        val nel2 = nel1.toIndexedSeq
        val nel3: NonEmpty[Int, IndexedSeq[Int]] = nel2
        typeEquals(nel2, nel3)
        nel2.value shouldBe IndexedSeq(1, 2, 3)
      }

      it("should preserve non-emptiness after .concat()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1.concat(Seq(4, 5, 6))
        typeEquals(nel1, nel2) shouldBe true
        nel2.value shouldBe Vector(1, 2, 3, 4, 5, 6)
      }

      it("should preserve non-emptiness after ++") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1 ++ Seq(4, 5, 6)
        typeEquals(nel1, nel2) shouldBe true
        nel2.value shouldBe Vector(1, 2, 3, 4, 5, 6)
      }

      it("should preserve non-emptiness after .groupBy()") {
        val Some(nel) = NonEmpty.from(Seq(1, 2, 3, 4, 5, 6))
        val grouped1 = nel.groupBy(_ % 2 == 0)
        val grouped2: NonEmpty[(Boolean, NonEmpty[Int, Seq[Int]]), immutable.Map[Boolean, NonEmpty[Int, Seq[Int]]]] = nel.groupBy(_ % 2 == 0)
        typeEquals(grouped1, grouped2)
        grouped1(true).toSeq.value shouldBe Seq(2, 4, 6)
        grouped1(false).toSeq.value shouldBe Seq(1, 3, 5)
      }

      it("should preserve non-emptiness after .groupMap()") {
        val Some(nel) = NonEmpty.from(Seq("hoge", "foo", "bar", "baz"))
        val grouped1 = nel.groupMap(_.length)(s => s"$s (${s.length})")
        val grouped2: NonEmpty[(Int, NonEmpty[String, Seq[String]]), immutable.Map[Int, NonEmpty[String, Seq[String]]]] = nel.groupMap(_.length)(s => s"$s (${s.length})")
        typeEquals(grouped1, grouped2)
        grouped1.size shouldBe 2
        grouped1(3).toSeq.value shouldBe Seq("foo (3)", "bar (3)", "baz (3)")
        grouped1(4).toSeq.value shouldBe Seq("hoge (4)")
      }

      it("should preserve non-emptiness after .groupMapReduce()") {
        val Some(nel) = NonEmpty.from(Seq("hoge", "foo", "bar", "baz"))
        val grouped1 = nel.groupMapReduce(_.length)(_ => 1)(_ + _)
        val grouped2: NonEmpty[(Int, Int), immutable.Map[Int, Int]] = nel.groupMapReduce(_.length)(_ => 1)(_ + _)
        typeEquals(grouped1, grouped2)
        grouped1.size shouldBe 2
        grouped1(3) shouldBe 3
        grouped1(4) shouldBe 1
      }

      it("should preserve non-emptiness after .grouped()") {
        val Some(nel) = NonEmpty.from(Vector(1, 2, 3, 4))
        val grouped1 = nel.grouped(2)
        val grouped2: Iterator[NonEmpty[Int, Vector[Int]]] = grouped1
        typeEquals(grouped1, grouped2)
        val vec = grouped1.toVector
        vec(0).toSeq.value shouldBe Seq(1, 2)
        vec(1).toSeq.value shouldBe Seq(3, 4)
      }

      it("should preserve non-emptiness after .map()") {
        val Some(nel1) = NonEmpty.from(Seq("hoge", "foo", "bar"))
        val nel2 = nel1.map(_.length)
        val nel3: NonEmpty[Int, Seq[Int]] = nel2
        typeEquals(nel2, nel3)
        nel2.toSeq.value shouldBe Seq(4, 3, 3)
      }

      it("should preserve non-emptiness after .scan()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3, 4)
        val nel2 = nel1.scan(0)(_ + _)
        val nel3: NonEmpty[Int, Vector[Int]] = nel2
        typeEquals(nel2, nel3)
        nel2.toSeq.value shouldBe Seq(0, 1, 3, 6, 10)
      }

      it("should preserve non-emptiness after .scanLeft()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3, 4)
        val nel2 = nel1.scanLeft("")(_ + _.toString)
        val nel3: NonEmpty[String, Vector[String]] = nel2
        typeEquals(nel2, nel3)
        nel2.toSeq.value shouldBe Seq("", "1", "12", "123", "1234")
      }

      it("should preserve non-emptiness after .scanRight()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3, 4)
        val nel2 = nel1.scanRight("")(_.toString + _)
        val nel3: NonEmpty[String, Vector[String]] = nel2
        typeEquals(nel2, nel3)
        nel2.toSeq.value shouldBe Seq("1234", "234", "34", "4", "")
      }

      it("should preserve non-emptiness after .sliding()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3, 4)
        val it1 = nel1.sliding(2).toSeq
        val it2 = nel1.sliding(2, 2).toSeq
        val nel3: NonEmpty[Int, Vector[Int]] = it1(0)
        val nel4: NonEmpty[Int, Vector[Int]] = it2(0)
        typeEquals(it1(0), nel3)
        typeEquals(it2(0), nel4)
        it1.map(_.value).toSeq shouldBe Seq(Vector(1, 2), Vector(2, 3), Vector(3, 4))
        it2.map(_.value).toSeq shouldBe Seq(Vector(1, 2), Vector(3, 4))
      }

      it("should preserve non-emptiness after .transpose()") {
        val nel1 = NonEmpty[Vector[List[Int]]](List(1, 2), List(3, 4))
        val transposed1 = nel1.transpose
        val transposed2: Vector[NonEmpty[Int, Vector[Int]]] = nel1.transpose
        typeEquals(transposed1, transposed2)
        transposed1.map(_.value) shouldBe Vector(
          Vector(1, 3),
          Vector(2, 4),
        )

        val nel2 = NonEmpty[Vector[List[Int]]](List[Int](), List[Int]())
        val transposed3 = nel2.transpose
        val transposed4: Vector[NonEmpty[Int, Vector[Int]]] = nel2.transpose
        typeEquals(transposed3, transposed4)
        transposed3.map(_.value) shouldBe Vector()
      }

      it("should preserve non-emptiness after .unzip()") {
        val nel1 = NonEmpty[Vector[(String, Int)]](
          "hoge" -> 4,
          "foo"  -> 3,
          "bar"  -> 3,
        )
        val (nel2, nel3) = nel1.unzip
        val nel4: NonEmpty[String, Vector[String]] = nel2
        val nel5: NonEmpty[Int, Vector[Int]] = nel3
        typeEquals(nel2, nel4)
        typeEquals(nel3, nel5)
        nel2.toSeq.value shouldBe Seq("hoge", "foo", "bar")
        nel3.toSeq.value shouldBe Seq(4, 3, 3)
      }

      it("should preserve non-emptiness after .unzip3()") {
        val nel1 = NonEmpty[Vector[(String, Int, Char)]](
          ("hoge", 4, 'h'),
          ("foo",  3, 'f'),
          ("bar",  3, 'b'),
        )
        val (nel2, nel3, nel4) = nel1.unzip3
        val nel5: NonEmpty[String, Vector[String]] = nel2
        val nel6: NonEmpty[Int, Vector[Int]] = nel3
        val nel7: NonEmpty[Char, Vector[Char]] = nel4
        typeEquals(nel2, nel5)
        typeEquals(nel3, nel6)
        typeEquals(nel4, nel7)
        nel2.toSeq.value shouldBe Seq("hoge", "foo", "bar")
        nel3.toSeq.value shouldBe Seq(4, 3, 3)
        nel4.toSeq.value shouldBe Seq('h', 'f', 'b')
      }

      it("should preserve non-emptiness after .zipAll()") {
        val nel1 = NonEmpty[Vector[String]]("hoge", "foo", "baz")
        val nel2 = NonEmpty[Vector[Int]](4, 3)
        val zipped1 = nel1.zipAll(nel2, "bar", 3)
        val zipped2 = nel2.zipAll(nel1, 3, "bar")
        val zipped3: NonEmpty[(String, Int), Vector[(String, Int)]] = zipped1
        val zipped4: NonEmpty[(Int, String), Vector[(Int, String)]] = zipped2
        typeEquals(zipped1, zipped3)
        typeEquals(zipped2, zipped4)
        zipped1.toSeq.value shouldBe Seq("hoge" -> 4, "foo" -> 3, "baz" -> 3)
        zipped2.toSeq.value shouldBe Seq(4 -> "hoge", 3 -> "foo", 3 -> "baz")
      }

      it("should preserve non-emptiness after .zipWithIndex()") {
        val nel1 = NonEmpty[Vector[String]]("hoge", "foo")
        val zipped1 = nel1.zipWithIndex
        val zipped2: NonEmpty[(String, Int), Vector[(String, Int)]] = zipped1
        typeEquals(zipped1, zipped2)
        zipped1.toSeq.value shouldBe Seq("hoge" -> 0, "foo" -> 1)
      }
    }

    describe("SeqOps") {
      it("should preserve non-emptiness after .prepended()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1.prepended(0)
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(0, 1, 2, 3)
      }

      it("should preserve non-emptiness after +:") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = 0 +: nel1
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(0, 1, 2, 3)
      }

      it("should preserve non-emptiness after .appended()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1.appended(4)
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(1, 2, 3, 4)
      }

      it("should preserve non-emptiness after :+") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1 :+ 4
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(1, 2, 3, 4)
      }

      it("should preserve non-emptiness after .prependedAll()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1.prependedAll(Seq(-1, 0))
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(-1, 0, 1, 2, 3)
      }

      it("should preserve non-emptiness after ++:") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = Seq(-1, 0) ++: nel1
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(-1, 0, 1, 2, 3)
      }

      it("should preserve non-emptiness after .appendedAll()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1.appendedAll(Seq(4, 5))
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(1, 2, 3, 4, 5)
      }

      it("should preserve non-emptiness after :++") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1 :++ Seq(4, 5)
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(1, 2, 3, 4, 5)
      }

      it("should preserve non-emptiness after .permutations()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val perm1 = nel1.permutations.toSeq
        typeEquals(Seq(nel1), perm1)
        perm1.map(_.value) should contain theSameElementsAs(Seq(
          Vector(1, 2, 3),
          Vector(1, 3, 2),
          Vector(2, 1, 3),
          Vector(2, 3, 1),
          Vector(3, 1, 2),
          Vector(3, 2, 1),
        ))
      }

      it("should preserve non-emptiness after .reverse()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1.reverse
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(3, 2, 1)
      }

      it("should preserve non-emptiness after .sortBy()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1.sortBy(_ * -1)
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(3, 2, 1)
      }

      it("should preserve non-emptiness after .sortWith()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1.sortWith(_ > _)
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(3, 2, 1)
      }

      it("should preserve non-emptiness after .sorted()") {
        val nel1 = NonEmpty[Vector[Int]](1, 3, 2)
        val nel2 = nel1.sorted
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(1, 2, 3)
      }

      it("should preserve non-emptiness after .updated()") {
        val nel1 = NonEmpty[Vector[Int]](1, 2, 3)
        val nel2 = nel1.updated(1, 20)
        typeEquals(nel1, nel2)
        nel2.value shouldBe Vector(1, 20, 3)
      }
    }

    describe("ListOps") {
      it("should preserve non-emptiness after ::") {
        val nel1 = NonEmpty[List[Int]](1, 2, 3)
        val nel2 = 0 :: nel1
        typeEquals(nel1, nel2)
        nel2.value shouldBe List(0, 1, 2, 3)
      }

      it("should preserve non-emptiness after :::") {
        val nel1 = NonEmpty[List[Int]](1, 2, 3)
        val nel2 = List(-2, -1, 0) ::: nel1
        typeEquals(nel1, nel2)
        nel2.value shouldBe List(-2, -1, 0, 1, 2, 3)
      }

      it("should preserve non-emptiness after .reverse_:::()") {
        val nel1 = NonEmpty[List[Int]](1, 2, 3)
        val nel2 = nel1.reverse_:::(List(4, 5, 6))
        typeEquals(nel1, nel2)
        nel2.value shouldBe List(6, 5, 4, 1, 2, 3)
      }

      it("should preserve non-emptiness after .mapConserve()") {
        case class Foo(n: Int)
        val nel1 = NonEmpty[List[Foo]](Foo(1), Foo(2), Foo(3))
        val nel2 = nel1.mapConserve(identity)
        val nel3 = nel1.mapConserve(_ => Foo(10))
        typeEquals(nel1, nel2)
        nel1.value should be theSameInstanceAs nel2.value
        typeEquals(nel1, nel3)
        nel3.value shouldBe List(Foo(10), Foo(10), Foo(10))
        nel1.value should not be theSameInstanceAs (nel3.value)
      }
    }

    describe("MapOps") {
      it("should preserve non-emptiness after .keySet") {
        locally {
          val nel1 = NonEmpty[Map[String, Int]]("hoge" -> 4, "foo" -> 3)
          val nel2 = nel1.keySet
          val nel3 = NonEmpty[Set[String]]("hoge", "foo")
          typeEquals(nel2, nel3)
          nel2.value shouldBe nel3.value
        }

        locally {
          val Some(nel1) = NonEmpty.from(scala.collection.Map("hoge" -> 4, "foo" -> 3))
          val nel2 = nel1.keySet
          val nel3: NonEmpty[String, scala.collection.Set[String]] = NonEmpty[Set[String]]("hoge", "foo")
          typeEquals(nel2, nel3)
          nel2.value shouldBe nel3.value
        }
      }

      it("should preserve non-emptiness after .keys") {
        locally {
          val nel1 = NonEmpty[Map[String, Int]]("hoge" -> 4, "foo" -> 3)
          val nel2 = nel1.keys
          val nel3: NonEmpty[String, Iterable[String]] = NonEmpty[List[String]]("hoge", "foo")
          typeEquals(nel2, nel3)
          nel2.value should contain theSameElementsAs nel3.value
        }
      }

      it("should preserve non-emptiness after .values") {
        locally {
          val nel1 = NonEmpty[Map[String, Int]]("hoge" -> 4, "foo" -> 3)
          val nel2 = nel1.values
          val nel3: NonEmpty[Int, Iterable[Int]] = NonEmpty[List[Int]](4, 3)
          typeEquals(nel2, nel3)
          nel2.value should contain theSameElementsAs nel3.value
        }
      }

      it("should preserve non-emptiness after .updated()") {
        locally {
          val nel1 = NonEmpty[Map[String, Int]]("hoge" -> 4, "foo" -> 3)
          val nel2 = nel1.updated("bar", 3)
          typeEquals(nel1, nel2)
          nel2.value shouldBe Map("hoge" -> 4, "foo" -> 3, "bar" -> 3)
        }
      }

      it("should preserve non-emptiness after +") {
        locally {
          val nel1 = NonEmpty[Map[String, Int]]("hoge" -> 4, "foo" -> 3)
          val nel2 = nel1 + ("bar" -> 3)
          typeEquals(nel1, nel2)
          nel2.value shouldBe Map("hoge" -> 4, "foo" -> 3, "bar" -> 3)
        }
      }

      it("should preserve non-emptiness after .transform()") {
        locally {
          val nel1 = NonEmpty[Map[String, Int]]("hoge" -> 4, "foo" -> 3)
          val nel2 = nel1.transform(_ + _.toString)
          val nel3: NonEmpty[(String, String), Map[String, String]] = nel2
          typeEquals(nel2, nel3)
          nel2.value shouldBe Map("hoge" -> "hoge4", "foo" -> "foo3")
        }
      }
    }

    describe("SetOps") {
      it("should preserve non-emptiness after .union()") {
        val nel1 = NonEmpty[Set[Int]](1, 2, 3)
        val nel2 = nel1.union(Set(2, 4, 6))
        val nel3: NonEmpty[Int, Set[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
        nel2.value shouldBe Set(1, 2, 3, 4, 6)
      }

      it("should preserve non-emptiness after |") {
        val nel1 = NonEmpty[Set[Int]](1, 2, 3)
        val nel2 = nel1 | Set(2, 4, 6)
        val nel3: NonEmpty[Int, Set[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
        nel2.value shouldBe Set(1, 2, 3, 4, 6)
      }

      it("should preserve non-emptiness after .incl()") {
        val nel1 = NonEmpty[Set[Int]](1, 2, 3)
        val nel2 = nel1.incl(4)
        val nel3: NonEmpty[Int, Set[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
        nel2.value shouldBe Set(1, 2, 3, 4)
      }

      it("should preserve non-emptiness after +") {
        val nel1 = NonEmpty[Set[Int]](1, 2, 3)
        val nel2 = nel1 + 4
        val nel3: NonEmpty[Int, Set[Int]] = nel1
        typeEquals(nel1, nel2) shouldBe true
        typeEquals(nel2, nel3) shouldBe true
        nel2.value shouldBe Set(1, 2, 3, 4)
      }
    }

    describe("LazyListOps") {
      it("should preserve non-emptiness after .force") {
        var called: Int = 0
        val ll = LazyList.continually({
          called += 1
          called
        })
        val Some(nel1) = NonEmpty.from(ll.take(5))
        called shouldBe 1
        val nel2 = nel1.force
        called shouldBe 5
        val nel3: NonEmpty[Int, LazyList[Int]] = nel1
        typeEquals(nel1, nel2)
        typeEquals(nel2, nel3)
        nel1.value should be theSameInstanceAs nel2.value
        nel1.value.toVector shouldBe Vector(1, 2, 3, 4, 5)
      }

      it("should preserve non-emptiness after .lazyAppendAll") {
        var called: Int = 0
        val ll = LazyList.continually({
          called += 1
          called
        })
        val Some(nel1) = NonEmpty.from(ll.take(3))
        called shouldBe 1
        val nel2 = nel1.lazyAppendedAll(Seq(4, 5, 6))
        called shouldBe 1
        val nel3: NonEmpty[Int, LazyList[Int]] = nel1
        typeEquals(nel1, nel2)
        typeEquals(nel2, nel3)
        nel2.value.toVector shouldBe Vector(1, 2, 3, 4, 5, 6)
        called shouldBe 3
      }

      it("should preserve non-emptiness after #::") {
        var called: Int = 0
        val ll = LazyList.continually({
          called += 1
          called
        })
        val Some(nel1) = NonEmpty.from(ll.take(3))
        called shouldBe 1
        val nel2 = 0 #:: nel1
        called shouldBe 1
        val nel3: NonEmpty[Int, LazyList[Int]] = nel1
        typeEquals(nel1, nel2)
        typeEquals(nel2, nel3)
        nel2.value.toVector shouldBe Vector(0, 1, 2, 3)
        called shouldBe 3
      }

      it("should preserve non-emptiness after #:::") {
        var called: Int = 0
        val ll = LazyList.continually({
          called += 1
          called
        })
        val Some(nel1) = NonEmpty.from(ll.take(3))
        called shouldBe 1
        val ll2 = LazyList(-2, -1, 0)
        val nel2 = ll2 #::: nel1
        called shouldBe 1
        val nel3: NonEmpty[Int, LazyList[Int]] = nel1
        typeEquals(nel1, nel2)
        typeEquals(nel2, nel3)
        nel2.value.toVector shouldBe Vector(-2, -1, 0, 1, 2, 3)
        called shouldBe 3
      }
    }

    describe("QueueOps") {
      it("should preserve non-emptiness after .enqueue()") {
        val nel1 = NonEmpty[immutable.Queue[Int]](1, 2, 3)
        val nel2 = nel1.enqueue(4)
        typeEquals(nel1, nel2)
        nel2.value shouldBe immutable.Queue(1, 2, 3, 4)
      }

      it("should preserve non-emptiness after .enqueueAll()") {
        val nel1 = NonEmpty[immutable.Queue[Int]](1, 2, 3)
        val nel2 = nel1.enqueueAll(Seq(4, 5))
        typeEquals(nel1, nel2)
        nel2.value shouldBe immutable.Queue(1, 2, 3, 4, 5)
      }
    }

    describe("StringOps") {
      it("should preserve non-emptiness after .concat()") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1.concat("bar")
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "foobar"
      }

      it("should preserve non-emptiness after ++") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1 ++ "bar"
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "foobar"
      }

      it("should preserve non-emptiness after .prepended()") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1.prepended('a')
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "afoo"
      }

      it("should preserve non-emptiness after +:") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = 'a' +: nes1
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "afoo"
      }

      it("should preserve non-emptiness after .prependedAll()") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1.prependedAll("bar")
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "barfoo"
      }

      it("should preserve non-emptiness after ++:") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = "bar" ++: nes1
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "barfoo"
      }

      it("should preserve non-emptiness after .appended()") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1.appended('x')
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "foox"
      }

      it("should preserve non-emptiness after :+") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1 :+ 'x'
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "foox"
      }

      it("should preserve non-emptiness after .appendedAll()") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1.appendedAll("bar")
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "foobar"
      }

      it("should preserve non-emptiness after :++") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1 :++ "bar"
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "foobar"
      }

      it("should preserve non-emptiness after .padTo()") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1.padTo(5, 'x')
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "fooxx"
      }

      it("should preserve non-emptiness after *") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1 * 3
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "foofoofoo"
      }

      it("should preserve non-emptiness after .capitalize()") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1.capitalize
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "Foo"
      }

      it("should preserve non-emptiness after .format()") {
        val Some(nes1) = NonEmpty.from("foo%s")
        val nes2 = nes1.format("bar")
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "foobar"
      }

      it("should preserve non-emptiness after .formatLocal()") {
        val Some(nes1) = NonEmpty.from("foo%s")
        val nes2 = nes1.formatLocal(java.util.Locale.ENGLISH, "bar")
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "foobar"
      }

      it("should preserve non-emptiness after .toLowerCase()") {
        locally {
          val Some(nel) = NonEmpty.from("FOO")
          val s1 = nel.toLowerCase
          val s2: NonEmpty[Char, immutable.WrappedString] = s1
          typeEquals(s1, s2)
          s1.value.unwrap shouldBe "foo"
        }

        locally {
          val Some(nel) = NonEmpty.from("FOO")
          val s1 = nel.toLowerCase(java.util.Locale.ENGLISH)
          val s2: NonEmpty[Char, immutable.WrappedString] = s1
          typeEquals(s1, s2)
          s1.value.unwrap shouldBe "foo"
        }
      }

      it("should preserve non-emptiness after .toUpperCase()") {
        locally {
          val Some(nel) = NonEmpty.from("foo")
          val s1 = nel.toUpperCase
          val s2: NonEmpty[Char, immutable.WrappedString] = s1
          typeEquals(s1, s2)
          s1.value.unwrap shouldBe "FOO"
        }

        locally {
          val Some(nel) = NonEmpty.from("foo")
          val s1 = nel.toUpperCase(java.util.Locale.ENGLISH)
          val s2: NonEmpty[Char, immutable.WrappedString] = s1
          typeEquals(s1, s2)
          s1.value.unwrap shouldBe "FOO"
        }
      }

      it("should preserve non-emptiness after .reverse") {
        val Some(nes1) = NonEmpty.from("foo")
        val nes2 = nes1.reverse
        typeEquals(nes1, nes2) shouldBe true
        nes2.value.unwrap shouldBe "oof"
      }

      it("should preserve non-emptiness after .grouped()") {
        val Some(nel) = NonEmpty.from("foo")
        val grouped1 = nel.grouped(2)
        val grouped2: Iterator[NonEmpty[Char, immutable.WrappedString]] = grouped1
        typeEquals(grouped1, grouped2)
        grouped1.toVector.map(s => s: String) shouldBe Vector("fo", "o")
      }
    }

    describe("NonEmpty[Char, WrappedString]") {
      it("should provide String methods") {
        val Some(nel) = NonEmpty.from("foo")
        val s1 = nel.replaceAll("o", "x")
        val s2: String = s1
        typeEquals(s1, s2)
        s1 shouldBe "fxx"
      }
    }

    it("should lose emptiness if the number of elements may reduce") {
      assertTypeError("implicitly[MaybeEmpty[NonEmpty[Int, Seq[Int]]]]")

      locally {
        val nel = NonEmpty[Seq[Seq[Int]]](Seq(1, 2), Seq(3, 4))
        val l = nel.flatten
        maybeEmpty(l)
        assertTypeError("val x: NonEmpty[Int, Seq[Int]] = l")
      }

      locally {
        val nel = NonEmpty[Seq[Int]](1, 2, 3)
        val l = nel.drop(1)
        maybeEmpty(l)
        assertTypeError("val x: NonEmpty[Int, Seq[Int]] = l")
      }

      locally {
        val nel = NonEmpty[Seq[Int]](1, 2, 3)
        val l = nel.take(1)
        maybeEmpty(l)
        assertTypeError("val x: NonEmpty[Int, Seq[Int]] = l")
      }
    }
  }

  describe("NonEmpty[Char, WrappedString]") {
    it("should provide methods in java.lang.String") {
      val Some(nes) = NonEmpty.from("  foo  ")
      val s = nes.trim
      s shouldBe "foo"
      typeEquals(s, "foo")

      nes.charAt(2) shouldBe 'f'
    }

    it("should provide methods in StringOps") {
      val Some(nes) = NonEmpty.from("foo")
      nes.split('o') shouldBe Array("f")
      val s = nes.take(2)
      s shouldBe "fo"
      typeEquals(s, "fo")
    }

    it("should provide methods in WrappedString") {
      val Some(nes) = NonEmpty.from("foobar")
      nes.segmentLength(_ == 'f') shouldBe 1
    }
  }

  describe("Compatibility with refined") {
    type PredNonEmpty = refined.collection.NonEmpty

    def compatibleWithRefined[A, CC[X] <: Iterable[X], F[_, _]](
      ne: F[CC[A], PredNonEmpty]
    )(implicit rt: RefType[F]): Unit = { /* ok */ }

    it("should be compatible with refined") {
      locally {
        val nel = NonEmpty[Vector[Int]](1)
        it should behave like compatibleWithRefined(nel)
      }

      locally {
        val Right(ref) = refineV[PredNonEmpty](Vector(1, 2, 3))
        val nel1 = NonEmpty.fromRefined(ref)
        val nel2: NonEmpty[Int, Vector[Int]] = ref
        typeEquals(nel1, nel2)

        val ref1: refined.api.Refined[Vector[Int], PredNonEmpty] = nel1
        val ref2: refined.api.Refined[Iterable[Int], PredNonEmpty] = nel1
        val ref3 = nel1.toRefined
        typeEquals(ref1, ref3)
      }

      locally {
        val Right(ref) = refineV[PredNonEmpty](Map(1 -> "foo"))
        val nel1 = NonEmpty.fromRefined(ref)
        val nel2: NonEmpty[(Int, String), Map[Int, String]] = ref
        typeEquals(nel1, nel2)

        val ref1: refined.api.Refined[Map[Int, String], PredNonEmpty] = nel1
        val ref2: refined.api.Refined[Iterable[(Int, String)], PredNonEmpty] = nel1
        val ref3 = nel1.toRefined
        typeEquals(ref1, ref3)
      }

      locally {
        val Right(ref) = refineV[PredNonEmpty](immutable.TreeMap(1 -> "foo"))
        val nel1 = NonEmpty.fromRefined(ref)
        val nel2: NonEmpty[(Int, String), immutable.TreeMap[Int, String]] = ref
        typeEquals(nel1, nel2)

        val ref1: refined.api.Refined[immutable.TreeMap[Int, String], PredNonEmpty] = nel1
        val ref2: refined.api.Refined[Iterable[(Int, String)], PredNonEmpty] = nel1
        val ref3 = nel1.toRefined
        typeEquals(ref1, ref3)
      }

      locally {
        val Right(ref) = refineV[PredNonEmpty](immutable.BitSet(1, 2, 3))
        val nel1 = NonEmpty.fromRefined(ref)
        val nel2: NonEmpty[Int, immutable.BitSet] = ref
        typeEquals(nel1, nel2)

        val ref1: refined.api.Refined[immutable.BitSet, PredNonEmpty] = nel1
        val ref2: refined.api.Refined[Iterable[Int], PredNonEmpty] = nel1
        val ref3 = nel1.toRefined
        typeEquals(ref1, ref3)
      }

      locally {
        val Right(ref) = refineV[PredNonEmpty]("foo")
        val nel1 = NonEmpty.fromRefined(ref)
        val nel2: NonEmpty[Char, immutable.WrappedString] = ref
        typeEquals(nel1, nel2)

        val ref1: refined.api.Refined[String, PredNonEmpty] = nel1
        val ref2: refined.api.Refined[Iterable[Char], PredNonEmpty] = nel1
        val ref3 = nel1.toRefined
        typeEquals(ref1, ref3)
      }
    }
  }
}
