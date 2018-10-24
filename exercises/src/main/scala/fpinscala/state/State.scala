package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // We generate an integer >= 0 and divide it by one higher than the
  // maximum. This is just one possible solution.
  def double2(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def double4: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i, rng2) => (i % 2 == 0, rng2) }

  def intDouble2(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    (rng: RNG) => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    val nonNeg = if (i < 0) -(i + 1) else i
    (nonNeg, rng2)
  }

  def nonNegativeEven: Rand[Int] = map[Int, Int](nonNegativeInt)(i => i - i % 2)

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(a => a / (Int.MaxValue.toDouble + 1))(rng)

  val double2: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2[A, B, Tuple2[A, B]](ra, rb) { (a: A, b: B) =>
      Tuple2.apply(a, b)
    }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = int(rng)
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case _ if count <= 0 => (List.empty, rng)
    case n =>
      val (i, rng2) = rng.nextInt
      val (l, rng3) = ints(n - 1)(rng2)
      (i :: l, rng3)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (ra, ras) =>
      map2(ra, ras) { (a, as) =>
        a :: as
      }
    }

  def _ints(count: Int): Rand[List[Int]] = {
    val a = List.fill(count)(int)
    sequence[Int](a)
  }

  def flatMap[A, B](f: Rand[A])(g: A => RNG => (B, RNG)): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      val c = g(a)
      val b = c(rng2)
      b
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

  def __map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
      }
    }
}

import State._

import scala.annotation.tailrec

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State({ s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def update: Input => Machine => Machine = { i: Input =>
    { m: Machine =>
      (i, m) match {
        case (_, Machine(_, 0, _))        => m
        case (Turn, Machine(true, _, _))  => m
        case (Coin, Machine(false, _, _)) => m
        case (Turn, Machine(false, candies, _)) =>
          m.copy(locked = true, candies = candies - 1)
        case (Coin, Machine(true, _, coins)) =>
          m.copy(locked = false, coins = coins + 1)
      }
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
//    val f1: (Machine => Machine) => State[Machine, Unit] =
//      (mm: Machine => Machine) => modify(mm)
//    val f3: Input => State[Machine, Unit] = f1.compose(update)
//
//    val states2: List[State[Machine, Unit]] = inputs.map(f3)

//    modify[Machine] _ compose update

    val states3: List[State[Machine, Unit]] = inputs.map { i: Input =>
      val newStateFunction = update(i)
      modify(newStateFunction)
    }

    val states: List[State[Machine, Unit]] = inputs.map(
      ((mm: Machine => Machine) => modify[Machine](mm)).compose(update)
    )

    val states2: List[State[Machine, Unit]] = inputs.map(
      (modify[Machine](_)).compose(update)
    )

    val f = ((mm: Machine => Machine) => modify[Machine](mm))
    val g = f.compose(update)
    val g2 = { (i: Input) => modify[Machine](update(i)) }
    val states4: List[State[Machine, Unit]] = inputs.map(g)
    val states5: List[State[Machine, Unit]] = inputs.map(g2)

    sequence(states)
      .flatMap { _ =>
        get
      }
      .map { s =>
        (s.coins, s.candies)
      }
  }

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def sequenceR[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    @tailrec
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil    => (acc.reverse, s)
        case h :: t => h.run(s) match { case (a, s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s, sas, List()))
  }

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()


  def modify_2[S](f: S => S): State[S, Unit] = {
    State[S, S](s => (s, s)).flatMap { s2 =>
      State[S, Unit](_ => ((), f(s2)))
    }
  }

//  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
//    val s: List[State[Machine, Unit]] = inputs.map { input =>
//      ???
//    }
//
//    val seq = sequence(s)
//    seq.flatMap { s2 =>
//      get
//    }.map { s2 =>
//      s2
//      ???
//    }
//    ???
//  }

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
