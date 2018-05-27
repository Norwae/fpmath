package com.github.norwae.fpmath

import com.github.norwae.fpmath.fp.FPInteger
import com.github.norwae.fpmath.typelevel.Integral.{One, Succ, Zero}
import org.scalatest.{FlatSpec, Matchers}

class MultiplicationTest extends FlatSpec with Matchers {
  type Two = Succ[One]

  "Multiplying fp numbers" should "work on plain numbers" in {
    val f0: FPInteger[Zero] = 7
    val f1: FPInteger[Zero] = 3

    f0 * f1 shouldEqual FPInteger[Zero](21)
  }

  it should "work on scalar * fp" in {
    val f0 = 7
    val f1 = FPInteger[One](30)

    f0 * f1 shouldEqual FPInteger[One](210)
  }

  it should "work on fp * fp (homogenous)" in {
    val f0 = FPInteger[One](70)
    val f1 = FPInteger[One](30)

    f0 * f1 shouldEqual FPInteger[Two](2100)
  }

  it should "work on fp * fp (heterogenous)" in {
    val f0 = FPInteger[Two](700)
    val f1 = FPInteger[One](30)

    f0 * f1 shouldEqual FPInteger[Succ[Two]](21000)
  }
}
