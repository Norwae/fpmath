package com.github.norwae.fpmath

import com.github.norwae.fpmath.fp.FPInteger
import com.github.norwae.fpmath.typelevel.Integral.{One, Zero}
import org.scalatest.{FlatSpec, Matchers}

class ToDoubleConversionTest extends FlatSpec with Matchers {
  type Eight = One#Twice#Twice#Twice

  "Converting fp values to double" should "work for offset-0 numbers" in {

    val nr = FPInteger[Zero](199)
    nr.doubleValue shouldEqual 199
  }

  it should "work for offset one" in {
    val nr = FPInteger[One](15)
    nr.doubleValue shouldEqual 1.5
  }

  it should "work for high offsets" in {
    val nr = FPInteger[Eight](100000000)
    nr.doubleValue shouldEqual 1
  }

  it should "work for negative offsets" in {
    val nr = FPInteger[Eight#Negate](1)
    nr.doubleValue shouldEqual 100000000
  }
}
