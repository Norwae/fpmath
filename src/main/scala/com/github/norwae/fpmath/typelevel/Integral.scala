package com.github.norwae.fpmath.typelevel

import scala.language.higherKinds

sealed trait Integral {
  type S <: Integral
  type P <: Integral
  type Plus[A <: Integral] <: Integral
  type Minus[A <: Integral] <: Integral
  type Negate <: Integral

  type Twice = Plus[this.type]
}

object Integral {

  trait Zero extends Integral {
    override type S = Succ[Zero]
    override type P = Pred[Zero]
    override type Minus[A <: Integral] = A#Negate
    override type Plus[A <: Integral] = A
    override type Negate = Zero
  }


  trait Pred[N <: Integral] extends Integral {
    override type S = N
    override type P = Pred[Pred[N]]
    override type Plus[A <: Integral] = N#Plus[A#P]
    override type Minus[A <: Integral] = N#Minus[A#S]
    override type Negate = Succ[N#Negate]
  }

  trait Succ[N <: Integral] extends Integral {
    override type S = Succ[Succ[N]]
    override type P = N
    override type Plus[A <: Integral] = N#Plus[A#S]
    override type Minus[A <: Integral] = N#Minus[A#P]
    override type Negate = Pred[N#Negate]
  }

  type One = Zero#S
}