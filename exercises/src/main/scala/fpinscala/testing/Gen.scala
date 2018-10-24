package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.RNG.Rand

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

//trait Prop {
////  def &&(p: Prop): Prop = new Prop {
////    override def check: Boolean = Prop.this.check && p.check
////  }
//  def check: Result
//}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop =
    Prop { (max, testCases, rng) =>
      run(max, testCases, rng) match {
        case Passed | Proved => p.run(max, testCases, rng)
        case f: Falsified    => f
      }
    }

  def ||(p: Prop): Prop =
    Prop { (max, testCases, rng) =>
      run(max, testCases, rng) match {
        case Passed | Proved   => Passed
        case Falsified(msg, _) => p.tag(msg).run(max, testCases, rng)
      }
    }

  def tag(msg: String): Prop =
    Prop { (max, testCases, rng) =>
      run(max, testCases, rng) match {
        case Falsified(failureMessage, successes) =>
          Falsified(msg + "\n" + failureMessage, successes)
        case Passed | Proved => Passed
      }
    }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int
  type TestCases = Int
//  type Result = Either[(FailedCase, SuccessCount), SuccessCount]
//  type Result = Option[(FailedCase, SuccessCount)]

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(
      failure: FailedCase,
      successes: SuccessCount
  ) extends Result {
    override def isFalsified: Boolean = true
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    randomStream(gen)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, exception: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${exception.getMessage}\n" +
      s"stack trace:\n ${exception.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props
          .map(p =>
            Prop { (max, n, rng) =>
              p.run(max, casesPerSize, rng)
          })
          .toList
          .reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests: \n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println("+ OK, proved property.")
    }
  }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get() == Par.unit(2)(ES).get())

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  val p2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3 = Prop.check {
    equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))(ES).get
  }

  val S = weighted(choose(1, 4).map(Executors.newFixedThreadPool) -> 0,
                   unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar2(Gen.unit(()))(_ => p)

  val p2_2 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val pint = Gen.choose(0, 10).map(Par.unit)
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  val forkProp = Prop.forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"
}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

//  def listOf: SGen[List[A]] = Gen.listOf(this)
//  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = this.map2(g)((_, _))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.int).map(i => start + i % (stopExclusive - start)))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean2: Gen[Boolean] = Gen(State(RNG.boolean))

  val boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val l = List.fill(n)(g.sample)
    Gen(State.sequence(l))
  }

  val uniform: Gen[Double] = Gen(State(RNG.double))

  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start,
           if (stopExclusive % 2 == 0) stopExclusive - 1 else stopExclusive)
      .map(n => if (n % 2 != 0) n + 1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start,
           if (stopExclusive % 2 != 0) stopExclusive - 1 else stopExclusive)
      .map(n => if (n % 2 == 0) n + 1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] =
    for {
      i <- choose(from, to)
      j <- if (i % 2 == 0) even(from, to) else odd(from, to)
    } yield (i, j)

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List[A]()))((a, b) => a.map2(b)(_ :: _))

  def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) gen1 else gen2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double2).flatMap(d =>
      if (d < threshold) g1._1.sample else g2._1.sample))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  val maxProp2 = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val sortedProp: Prop = forAll(listOf(smallInt)) { ns =>
    val nss = ns.sorted
    nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
      case (n1, n2) => n2 < n1
    }
  }

  object ** {
    def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
  }

  lazy val pint2: Gen[Par[Int]] = choose(-100, 100)
    .listOfN(choose(0, 20))
    .map(l =>
      l.foldLeft(Par.unit(0))((p, i) =>
        Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

  val isEven = (i: Int) => i % 2 == 0
  val takeWhileProp =
    Prop.forAll(Gen.listOf(smallInt))(ns => ns.takeWhile(isEven).forall(isEven))

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => _ => i)
}

//trait Gen[A] {
//  def map[A, B](f: A => B): Gen[B] = ???
//  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
//}

//trait SGen[+A] {}
case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen({ n =>
      forSize(n).flatMap(f(_).forSize(n))
    })

  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_).map(f))
}
