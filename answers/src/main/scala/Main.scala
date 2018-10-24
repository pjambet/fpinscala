import java.util.concurrent.{Callable, Executors}

import fpinscala.laziness._
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.state
import fpinscala.state.RNG.Simple
import fpinscala.state.State.Rand
import fpinscala.testing.{Gen, Prop}
import fpinscala.parallelism.Nonblocking
import fpinscala.parsing.JSONExample

object Main {

  def loud(i: Int): Int = {
    println(s"RETURNING $i")
    i
  }

  def f(a: Int, b: => Int): Int = {
    lazy val b2 = b
    val r = a + b
    println(s"summing $a and $b: $r")
    r
  }

  def f2(s: String): () => String = () => {
    Thread.sleep(1000)
    s
  }

  def main(args: Array[String]): Unit = {
//    val ints2: Stream[Int] =
//      Stream.cons(loud(1), Stream.cons(loud(2), Stream.empty))
//    val ints5: Stream[Int] = Stream.cons(
//      loud(1),
//      Stream.cons(
//        loud(2),
//        Stream.cons(loud(3),
//                    Stream.cons(loud(4), Stream.cons(loud(5), Stream.empty)))))
//
//    val s = ints5.map(_ + 10).filter(_ % 2 == 0)
//    s.toList
//    val r = new RNG.Simple(42)
//    val r2 = RNG.nonNegativeEven
//    println(r2(r))

//    val a = Par.lazyUnit(42 + 1)
//    val S = Executors.newFixedThreadPool(4)
//    println(Par.equal(S)(a, Par.fork(a)))
//    println(Examples.sum(Vector(1, 2, 3)))
//    val S = Executors.newFixedThreadPool(4)
//    val r = Par.run[Int](S) _
//    println(r(Examples.sum2(Vector(1, 2, 3))).get())

//    val f = S.submit { new Callable[String] { def call = "Hello" } }
//    f.get

//    val echoer = Actor[() => String](S) {
//      msg => println(s"\n\nGot: ${msg()}")
//    }
//    echoer ! (() => "foo")
//
//    (1 to 5).foreach(i => echoer ! f2(s"s$i"))


//    Prop.run(Gen.maxProp1)

//    Prop.run(Prop.forkProp)
    val s: State[RNG, Int] = State.unit[RNG, Int](1)
    val rng1 = Simple(43)
    val rng = Simple(42)

//    s.run(rng)

//    val a = s.flatMap((_: Int) => State.get[RNG]).flatMap(r => State.set[RNG](rng))
//    a(rng)


//    println(s)
    JSONExample.test
  }

}
