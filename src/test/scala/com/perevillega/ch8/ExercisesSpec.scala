package com.perevillega.ch8

import org.scalatest.{FlatSpec, Matchers}

class ExercisesSpec extends FlatSpec with Matchers {

  // testing api

  "Gen and Props" should "run the first test example" in {
    val smallInt = Gen.choose(-10,10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    val run = Prop.run(maxProp)
    println(run._1)
    run._2 shouldBe true
  }
}