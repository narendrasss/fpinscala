package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the shell, which you can fill in and modify while working through the chapter.
 */

// TestCases refers to the number of cases that should pass for the Prop to be considered as passing.
case class Prop(run: TestCases => Result) {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFailed: Boolean
  }
  case object Passed extends Result {
    def isFailed: Boolean = false
  }
  case class Failed(failure: FailedCase, successes: SuccessCount)
      extends Result {
    def isFailed: Boolean = true
  }

  def check: Result
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))
    )

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // simple effectful utility to see contents of Gen
  def print[A](g: Gen[A]): Unit =
    println(g.sample.run(RNG.Simple(5))._1)

  // If we can generate an Int in some range, can we generate
  // an (Int, Int) tuple in some range?
  // We can't -> we can only do a list of length 2 instead
  def intTuple(start: Int, stopExclusive: Int): Gen[List[Int]] =
    listOfN(2, choose(start, stopExclusive))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(first => if (first) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(
      State(RNG.double)
        .flatMap(p => if (p > threshold) g2._1.sample else g1._1.sample)
    )
  }

  def unionViaWeighted[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    weighted((g1, 0.5), (g2, 0.5))
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))
}

trait SGen[+A] {}
