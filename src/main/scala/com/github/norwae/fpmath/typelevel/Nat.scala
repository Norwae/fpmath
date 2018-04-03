package com.github.norwae.fpmath.typelevel

import scala.language.higherKinds

sealed trait Nat {
  type Plus[A <: Nat] <: Nat
  type Twice = Plus[this.type]
}

object Nat {
  trait Zero extends Nat {
    override type Plus[A <: Nat] = A
  }

  trait Succ[N <: Nat] extends Nat {
    override type Plus[A <: Nat] = N#Plus[Succ[A]]
  }

  type One = Succ[Zero]
}