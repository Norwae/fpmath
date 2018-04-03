package com.github.norwae.fpmath
package fp

import com.github.norwae.fpmath.typelevel.Nat.Zero
import typelevel._

import scala.language.implicitConversions

class FPInteger[N <: Nat](val value: Int) extends AnyVal {
  import FPInteger.simple
  def +(other: FPInteger[N]) = new FPInteger[N](value + other.value)
  def -(other: FPInteger[N]) = new FPInteger[N](value - other.value)

  def *[N2 <: Nat](other: FPInteger[N2]) = new FPInteger[N#Plus[N2]](value * other.value)

  def ===(other: FPInteger[N]): Boolean = value == other.value

  def rescale[N2 <: Nat](implicit conv: Converter[N, N2]): FPInteger[N2] = {
    val factor = conv.scaleFactor
    new FPInteger[N2](if (factor < 0) value / -factor else value * factor)
  }

  def doubleValue(implicit conv: Converter[Zero, N]): Double = {
    val factor = simple(1).rescale[N].value
    val d = value.toDouble
    d / factor
  }
}

object FPInteger {
  implicit def simple(int: Int): FPInteger[Zero] = new FPInteger[Zero](int)
  def apply[N <: Nat](int: Int) = new FPInteger[N](int)
}
