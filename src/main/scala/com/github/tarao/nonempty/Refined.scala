package com.github.tarao.nonempty

import eu.timepit.refined
import eu.timepit.refined.api.RefType
import scala.reflect.macros.blackbox

trait Refined[+T, P] extends Any {
  def value: T
}
object Refined {
  final class Impl[+T, P] private[Refined] (val value: T)
      extends Refined[T, P]

  private def unsafeApply[T, P](t: T): Refined[T, P] = new Impl(t)

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

  implicit class ToRefinedOp[+T, P](
    private val r: Refined[T, P]
  ) extends AnyVal {
    def toRefined[T1 >: T]: refined.api.Refined[T1, P] =
      implicitly[RefType[refined.api.Refined]].unsafeWrap(r.value)
  }
}
