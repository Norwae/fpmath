package com.github.norwae.fpmath
package fp

import com.github.norwae.fpmath.typelevel.Integral.Zero
import typelevel._

import scala.language.implicitConversions

class FPInteger[N <: Integral](val value: Int) extends AnyVal {
  import FPInteger.simple
  def +(other: FPInteger[N]) = new FPInteger[N](value + other.value)
  def -(other: FPInteger[N]) = new FPInteger[N](value - other.value)

  def *[N2 <: Integral](other: FPInteger[N2]) = new FPInteger[N#Plus[N2]](value * other.value)
  def /[N2 <: Integral](other: FPInteger[N2]) = new FPInteger[N#Minus[N2]](value / other.value)

  def ===(other: FPInteger[N]): Boolean = value == other.value

  def rescale[N2 <: Integral](implicit conv: Converter[N, N2]): FPInteger[N2] = {
    val factor = conv.scaleFactor
    new FPInteger[N2](if (factor < 0) value / -factor else value * factor)
  }


  override def toString: String = value.toString

  def doubleValue(implicit conv: Converter[Zero, N]): Double = {
    val factor = conv.scaleFactor match {
      case 0 ⇒ throw new ArithmeticException("Cannot convert to double")
      case f if f < 0 ⇒ 1.0 / -f
      case f ⇒ f.toDouble
    }

    val d = value.toDouble
    d / factor
  }
}

object FPInteger {
  implicit def simple(int: Int): FPInteger[Zero] = new FPInteger[Zero](int)
  def apply[N <: Integral](int: Int) = new FPInteger[N](int)
}
