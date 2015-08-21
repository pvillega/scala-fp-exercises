package com.perevillega.ch6

import org.scalatest.{Matchers, FlatSpec}

class ExercisesSpec extends FlatSpec with Matchers {

// Given the nature of random values I did not spend much time doing tests for them
// Solutions can be compared to the official ones: https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/state/State.scala

  "6.11" should "simulate the example machine" in {
    val input = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val machine = Machine(10, 5)
    val state = Machine.simulateMachine(input).run(machine)
    val (coins, candies) = state._1

    coins should be(14)
    candies should be(1)
  }
}
