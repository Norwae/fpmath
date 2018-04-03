package com.github.norwae.fpmath

import com.github.norwae.fpmath.fp.FPInteger
import com.github.norwae.fpmath.typelevel.Nat.{One, Succ, Zero}
import org.scalatest.{FlatSpec, Matchers}

class AdditionTest extends FlatSpec with Matchers {
  type Seven = One#Plus[One#Twice]#Plus[One#Twice#Twice]

  "Adding fp numbers" should "work for plain numbers" in {
    val f1: FPInteger[Zero] = 19
    val f2 = 81

    val x = f1 + f2
    x.value shouldEqual 100
  }

  it should "add two numbers of same fixed point" in {
    val f1 = FPInteger[Seven](190000000)
    val f2 = FPInteger[Seven](810000000)

    val x = f1 + f2
    x.value shouldEqual 1000000000
  }

  it should "forbid mixing different point offsets" in {
    """val f1 = FPInteger[Zero](19)
    val f2 = FPInteger[Seven](810000000)

    val x = f1 + f2""" shouldNot compile
  }
}
