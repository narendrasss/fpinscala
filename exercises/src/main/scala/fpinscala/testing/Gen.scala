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
case class Prop(run: (TestCases, RNG) => Result) {
  def check: Result = ???
}

object Prop {
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

  // Generate an infinite stream by repeatedly sampling a Gen
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMessage[A](testCase: A, error: Exception): String =
    s"test case: $testCase\n" +
      s"generated an exception: ${error.getMessage}\n" +
      s"stack trace:\n ${error.getStackTrace.mkString("\n")}"

  /**
    * 1. Generate an infinite random stream using the given Gen.
    * 2. Convert the stream into a stream of (TestCase, index) tuples.
    * 3. Take n test cases.
    * 4. Apply the predicate to each test case. If it passes, return
    *    Passed otherwise return Failed.
    * 5. Find the first Failed case.
    * 6. Return the Failed case, or if it doesn't exist, return Passed.
    */
  def forAll[A](gen: Gen[A])(pred: A => Boolean): Prop =
    (n, rng) =>
      randomStream(gen)(rng)
        .zip(Stream.from(0))
        .take(n)
        .map {
          case (testCase, index) =>
            try {
              if (pred(testCase)) Passed else Failed(testCase.toString, index)
            } catch {
              case error: Exception =>
                Failed(buildMessage(testCase, error), index)
            }
        }
        .find(_.isFailed)
        .getOrElse(Passed)
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
