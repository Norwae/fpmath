package com.github.norwae.fpmath.typelevel

import scala.language.higherKinds

sealed trait Integral {
  type S <: Integral
  type P <: Integral
  type Plus[A <: Integral] <: Integral
  type Negate <: Integral

  final type Twice = Plus[this.type]
  final type Minus[A <: Integral] = Plus[A#Negate]
}

object Integral {

  final class Zero private extends Integral {
    override type S = Succ[Zero]
    override type P = Pred[Zero]
    override type Plus[A <: Integral] = A
    override type Negate = Zero
  }


  final class Pred[N <: Integral] private extends Integral {
    override type S = N
    override type P = Pred[Pred[N]]
    override type Plus[A <: Integral] = N#Plus[A#P]
    override type Negate = Succ[N#Negate]
  }

  final class Succ[N <: Integral] private extends Integral {
    override type S = Succ[Succ[N]]
    override type P = N
    override type Plus[A <: Integral] = N#Plus[A#S]
    override type Negate = Pred[N#Negate]
  }

  type One = Zero#S
}