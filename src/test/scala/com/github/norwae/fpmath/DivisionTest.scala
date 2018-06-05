package com.github.norwae.fpmath

import com.github.norwae.fpmath.fp.FPInteger
import com.github.norwae.fpmath.typelevel.Integral.{One, Succ, Zero}
import org.scalatest.{FlatSpec, Matchers}

class DivisionTest extends FlatSpec with Matchers {
  type Two = Succ[One]

  "Dividing fp numbers" should "work on plain numbers" in {
    val f0: FPInteger[Zero] = 21
    val f1: FPInteger[Zero] = 3

    f0 / f1 shouldEqual FPInteger[Zero](7)
  }

  it should "work on  fp / scalar" in {
    val f0 = FPInteger[One](210)
    val f1 = 3

    f0 / f1 shouldEqual FPInteger[One](70)
  }

  it should "work on fp / fp (homogenous)" in {
    val f0 = FPInteger[One](210)
    val f1 = FPInteger[One](30)

    f0 / f1 shouldEqual FPInteger[Zero](7)
  }

  it should "work on fp / fp (heterogenous)" in {
    val f0 = FPInteger[One](2100)
    val f1 = FPInteger[Two](300)

    f0 / f1 shouldEqual FPInteger[One#Negate](7)
  }
}
