package com.github.norwae.fpmath

import com.github.norwae.fpmath.fp.FPInteger
import com.github.norwae.fpmath.typelevel.Integral.{One, Succ, Zero}
import org.scalatest.{FlatSpec, Matchers}

class EqualityTest extends FlatSpec with Matchers {

  "The strict equality operator" should "compare values" in {
    val f1 = FPInteger[Succ[Zero]](15) * 2
    val f2 = FPInteger[One](30)

    f1 === f2 shouldBe true
    f1 * 2 === f2 shouldBe false
  }

  // non-compilation test made impossible by inherting === from Matchers - thanks, scalactic

}
