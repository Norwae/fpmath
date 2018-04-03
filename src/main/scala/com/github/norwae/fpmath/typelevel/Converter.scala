package com.github.norwae.fpmath.typelevel

import com.github.norwae.fpmath.typelevel.Nat.Zero

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

sealed trait Converter[A <: Nat, B <: Nat] {
  val scaleFactor: Int
}

final class ConverterImpl(val scaleFactor: Int) extends Converter[Nothing, Nothing]
object Converter {
  implicit def provideConverter[N1 <: Nat, N2 <: Nat]: Converter[N1, N2] = macro _provideConverter[N1, N2]

  private val factors = Array(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)

  def _provideConverter[N1 <: Nat : c.WeakTypeTag, N2 <: Nat : c.WeakTypeTag](c: blackbox.Context): c.Expr[Converter[N1, N2]] = {
    import c.universe._
    val n1 = implicitly[WeakTypeTag[N1]].tpe.dealias
    val n2 = implicitly[WeakTypeTag[N2]].tpe.dealias
    val zero = implicitly[TypeTag[Zero]].tpe

    def count(typ: c.Type, cnt: Int): Int = {
      val isZero = typ == zero
      if (isZero) cnt else {
        val next = typ.typeArgs.head.dealias
        count(next, cnt + 1)
      }
    }

    val c1 = count(n1, 0)
    val c2 = count(n2, 0)
    val diff = c1 - c2

    val finalFactor = {
      val offset = Math.abs(diff)
      if (offset >= factors.length) 0
      else {
        val rawFactor = factors(offset)

        if (c1 >= c2) -rawFactor else rawFactor
      }
    }

    c.Expr(
      q"""new _root_.com.github.norwae.fpmath.typelevel.ConverterImpl($finalFactor).
         asInstanceOf[_root_.com.github.norwae.fpmath.typelevel.Converter[$n1, $n2]]
       """)
  }
}