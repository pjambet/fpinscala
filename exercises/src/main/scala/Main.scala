import java.util.Date
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Callable, ExecutorService, Executors, TimeUnit}

import fpinscala.applicative.AMonad.OptionT
import fpinscala.applicative._
import fpinscala.monads.Monad
import fpinscala.monoids.{ListFoldable, Monoid}
import fpinscala.parsing
import fpinscala.parallelism.Par
import fpinscala.parallelism.Nonblocking
import fpinscala.parallelism.Nonblocking.Future
import fpinscala.parsing.ReferenceTypes.Parser
import fpinscala.testing.{Gen, Prop}
import fpinscala.parsing._
import fpinscala.state.State

import scala.util.control.NonFatal

object Main extends App {

  def sum1(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum1(l) + sum1(r) // Recursively sum both halves and add the results together.
    }

  def sum(ints: IndexedSeq[Int]): Par.Par[Int] = {
    println(s"[${Thread.currentThread().getName}] sum")
    if (ints.length <= 1) {
      println(
        s"[${Thread.currentThread().getName}] 1 or 0 items, wrapping $ints with unit")
      Par.unit(ints.headOption getOrElse 0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      println(s"[${Thread.currentThread().getName}] Split result: $l, $r")
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  def sum_withParFold(ints: IndexedSeq[Int]): Par.Par[Int] = {
    Par.parFold(0, ints)(_ + _)(_ + _)
  }

  def max_withParFold(ints: IndexedSeq[Int]): Par.Par[Option[Int]] = {
    Par.parFold[Int, Option[Int]](None, ints) { (a, z) =>
      if (z.isDefined && z.get < a) Some(a) else z
    } { (t1, t2) =>
//      if (t1.isDefined )
      ???
    }
  }

  override def main(args: Array[String]): Unit = {
//    val numA: Parser[Int] = map(many(char('a)))(_.size)
    //    val numA2: Parser[Int] = char('a').many.map(_.size)

    val S = Executors.newFixedThreadPool(1)
    //    val S = Executors.newCachedThreadPool()

    //    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    //    val s2 = sum(Vector(1, 2, 3))
    //    val l = List.fill(2000)((Math.random() * 1000).toInt).toIndexedSeq
    //    val s2 = sum(l)

    //    println(Par.parFilter(l)(i => i <= 5)(S))

    //    println("Par created as a result of map2, running it now")
    //    val t0 = System.nanoTime()
    ////    val res = s2(S)
    //    val res = sum1(l)
    //    val t1 = System.nanoTime()
    //    println(res)
    //    println(s"Time: ${(t1 - t0) / 1000000}")

    //    val p: Par.Par[Int] = Par.map2(Par.lazyUnit(1), Par.lazyUnit(2))(_ + _)
    ////    val pp = Par.fork(p)
    //
    //    val u = Par.unit(1)
    //    val pu = Par.fork(u)
    //    u(S)
    //    pu(S)
    //
    //    val f = Par.fork(p)(S)
    //    println("Result:")
    //    println(f.get(5000, TimeUnit.MILLISECONDS))
    ////    pp(S).get()

    //    val t0 = System.nanoTime()
    //    val p = Nonblocking.Par.parMap(List.range(1, 10)) { i => Thread.sleep(1000); math.sqrt(i) }
    //    println(Nonblocking.Par.run(S)(p))
    ////    val p2 = List.range(1, 10).map { i => Thread.sleep(1000); math.sqrt(i) }
    ////    println(p2)
    //    val t1 = System.nanoTime()
    //    println(s"Time: ${(t1 - t0) / 1000000}")

    val plaz = Nonblocking.Par.lazyUnit(1)
    //    val c1 = new Callable[Int] {
    //      override def call(): Int = {
    //        println(s"[${Thread.currentThread().getName}] Starting to sort 1")
    //        val m = List.range(1, 1000000).min
    //        println(s"[${Thread.currentThread().getName}] Done with 1")
    //        m
    //      }
    //    }
    //    val c2 = new Callable[Int] {
    //      override def call(): Int = {
    //        println(s"[${Thread.currentThread().getName}] Starting to sort 2")
    //        val m = List.range(1, 1000000).min
    //        println(s"[${Thread.currentThread().getName}] Done with 2")
    //        m
    //      }
    //    }

    //    val f1 = S.submit(c1)
    //    val f2 = S.submit(c2)
    //    println()

    //    Nonblocking.Par.run(S)(Nonblocking.Par.map2(Nonblocking.Par.lazyUnit(1), Nonblocking.Par.lazyUnit(1))(_ + _))

    val f2 = (es: ExecutorService) => {
      new Future[Int] {
        def apply(cb: Int => Unit) = {
          es.submit(new Callable[Unit] {
            val unitA = (es: ExecutorService) => {
              new Future[Int] {
                def apply(cb: Int => Unit) = cb(1)
              }
            }

            def call: Unit = unitA(es)(cb)
          })
        }
      }
    }

//    val ff =
//      (es: ExecutorService) => {
//        new Future[Int] {
//          def apply(cb: Int => Unit) = {
//            println(s"[${Thread.currentThread().getName}] Submitting to $es")
//            es.submit(new Callable[Unit] {
//              def call: Unit = {
//                println(s"[${Thread.currentThread().getName}] Calling 1")
//                val forked = (es: ExecutorService) => {
//                  new Future[Int] {
//                    def apply(cb: Int => Unit) = {
//                      println(
//                        s"[${Thread.currentThread().getName}] Submitting to $es")
//                      es.submit(new Callable[Unit] {
//                        def call: Unit = {
//                          println(
//                            s"[${Thread.currentThread().getName}] Calling 2")
//                          val unitA = (_: ExecutorService) => {
//                            new Future[Int] {
//                              def apply(cb: Int => Unit) = cb(1)
//                            }
//                          }
//                          unitA(es)(cb)
//                        }
//                      })
//                      println(s"[${Thread.currentThread().getName}] I'm done")
//                    }
//                  }
//                }
//                forked(es)(cb)
//                println(s"[${Thread.currentThread().getName}] But for real")
//              }
//            })
//          }
//        }
//      }

//    val fmap = {
//      val f: Int => String = _.toString
//      es: ExecutorService =>
//        {
//          new Future[String] {
//            val forkedTwice = (es: ExecutorService) => {
//              new Future[Int] {
//                def apply(cb: Int => Unit) = {
//                  println(
//                    s"[${Thread.currentThread().getName}] Submitting to $es")
//                  es.submit(new Callable[Unit] {
//                    def call: Unit = {
//                      println(s"[${Thread.currentThread().getName}] Calling 1")
//                      val forkedOnce = (es: ExecutorService) => {
//                        new Future[Int] {
//                          def apply(cb: Int => Unit) = {
//                            println(
//                              s"[${Thread.currentThread().getName}] Submitting to $es")
//                            es.submit(new Callable[Unit] {
//                              def call: Unit = {
//                                println(
//                                  s"[${Thread.currentThread().getName}] Calling 2")
//                                val unitA = (_: ExecutorService) => {
//                                  new Future[Int] {
//                                    def apply(cb: Int => Unit) = cb(1)
//                                  }
//                                }
//                                unitA(es)(cb)
//                              }
//                            })
//                            println(
//                              s"[${Thread.currentThread().getName}] I'm done")
//                          }
//                        }
//                      }
//                      forkedOnce(es)(cb)
//                      println(
//                        s"[${Thread.currentThread().getName}] But for real")
//                    }
//                  })
//                }
//              }
//            }
//
//            def apply(cb: String => Unit): Unit = {
//              forkedTwice(es) { a =>
//                es.submit(new Callable[Unit] {
//                  def call: Unit = cb(f(a))
//                })
//              }
//            }
//          }
//        }
//    }

//    println(Nonblocking.Par.run(S)(ff))
//    println(Nonblocking.Par.run(S)(fmap).getClass)

//    val numA: Parser[Int] = char('a').many.map(_.size)

    S.shutdown()
    S.awaitTermination(5000, TimeUnit.MILLISECONDS)

//    var seq: Seq[Int] = Seq.empty
//    var list: List[Int] = List.empty
//    var indexedSeq: IndexedSeq[Int] = IndexedSeq.empty[Int]
//    var vector: Vector[Int] = Vector.empty[Int]
//
//    val f3: Function1[Function1[IndexedSeq[Int], IndexedSeq[Int]], Unit] = ???
//    val f: Function1[Seq[Int], Vector[Int]] = ???
//    f3(f)
//    JSONExample.test
//    val P = fpinscala.parsing.Reference
//    import fpinscala.parsing.ReferenceTypes.Parser
//    import P._
////    val spaces = regex("\\s*".r) //char(' ').many.slice
//    val spaces = string(" ") or string(" ").many
//    val ps = (attempt(string("abra") ** spaces ** string("abra")) ** string(
//      "cadabra")) or (string("abra") ** spaces ** string("cadabra!"))
////    val ps = string("abra") ** spaces ** string("abra") ** spaces ** string("cadabra")
////    println(P.run(ps)("abra"))
//
//    val p1 = scope("magic spell") {
//      string("abra") ** spaces ** string("cadabra")
//    }
//    val p2 = scope("gibberish") { string("abba") ** spaces ** string("babba")
//    }
//    val p = p2 or p1
//
//    new AtomicBoolean()
//    println(P.run(p)("abra cAdabra"))
//    println(P.run(p)("abra cadabra"))

//    import fpinscala.monoids.Monoid
//
//    val prop = Monoid.monoidLaws(Monoid.intAddition, Gen.smallInt)
//    println(Prop.run(prop))
//
//    println(Monoid.count("foo de fafa"))
//
//    println(ListFoldable.foldRight(List(10, 12, 14))(List.empty[Int])(_ :: _))
//    println(ListFoldable.foldLeft(List(10, 12, 14))(List.empty[Int])((is, i) => i :: is))
//    val M: Monoid[Map[String, Map[String, Int]]] =
//      Monoid.mapMergeMonoid(Monoid.mapMergeMonoid(Monoid.intAddition))
//
//    val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
//    val m2 = Map("o1" -> Map("i2" -> 3))
//    println(M.op(m1, m2))
//
//    println(Monoid.bag(Vector("a", "rose", "is", "a", "rose")))
//
//    println(Monad.optionMonad.filterM[String](List("foo", "bar", "foo")) { str =>
//      if (str == "foo") Some(true) else Some(false)
//    })
//
//    println(Monad.stateMonad[Int].map2(State.unit("1"), State.unit("2"))(_ + _).run(0))

//    println(AMonad.stateMonad[Int].unit(1))
    val F: Applicative[Option] = new Applicative[Option] {
      override def unit[A](a: => A): Option[A] = Option(a)

      override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
        fa.flatMap(a => fab.map(f => f(a)))
    }
    val depts: Map[String, String] = Map("pierre" -> "it")
    val salaries: Map[String, Double] = Map("pierre" -> 10)
    val o: Option[String] =
      F.map2(depts.get("pierre"), salaries.get("pierre"))((dept, salary) =>
        s"Pierre in $dept makes $salary per year")

//    println(o)
//
//    val st = Applicative.streamApplicative.sequence(
//      List(Applicative.streamApplicative.unit(1),
//           Applicative.streamApplicative.unit(2),
//           Stream(10, 12)))
//    println(st)

    case class WebForm(name: String, birthdate: Date, phoneNumber: String)
    def validName(name: String): Validation[String, String] =
      if (name != "") Success(name)
      else Failure("Name cannot be empty", Vector.empty)

    def validName2(name: String): Either[String, String] =
      validName(name) match {
        case Failure(h, t) => Left(h)
        case Success(s)    => Right(s)
      }

    def validBirthdate(birthdate: String): Validation[String, Date] =
      try {
        import java.text._
        Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
      } catch {
        case NonFatal(e) =>
          Failure("Birthdate must be in the form yyyy-MM-dd", Vector.empty)
      }

    def validBirthdate2(birthdate: String): Either[String, Date] =
      validBirthdate(birthdate) match {
        case Failure(h, t) => Left(h)
        case Success(s)    => Right(s)
      }

    def validPhone(phoneNumber: String): Validation[String, String] =
      if (phoneNumber.matches("[0-9]{10}"))
        Success(phoneNumber)
      else Failure("Phone number must be 10 digits", Vector.empty)

    def validPhone2(phoneNumber: String): Either[String, String] =
      validPhone(phoneNumber) match {
        case Failure(h, t) => Left(h)
        case Success(s)    => Right(s)
      }

    def validWebForm(name: String,
                     birthdate: String,
                     phone: String): Validation[String, WebForm] =
      Applicative.validationApplicative.map3(validName(name),
                                             validBirthdate(birthdate),
                                             validPhone(phone))(WebForm)

    def validWebForm2(name: String,
                      birthdate: String,
                      phone: String): Either[String, WebForm] =
      AMonad.eitherMonad.map3(validName2(name),
                              validBirthdate2(birthdate),
                              validPhone2(phone))(WebForm)

//    println(validWebForm("", "12/12/1989", "16467275376"))
//    println(validWebForm2("", "12/12/1989", "16467275376"))
//    Array(1)
//    Set(1, 2, 3, 4, 5)
//    Some(1).flatMap(_ => Some(2))
//
//    println(Traverse.listTraverse.traverse[Option, Int, Int](List(10, 20))(i =>
//      Option(i * 2))(Applicative.optionApplicative))
//
//    println("-----")
//    val l = Applicative.listApplicative.apply(List[Int => Int](_ + 1, _ + 2))(List(1))
//    println(s"l: $l")
//    println("-----")
//    val r = Applicative.listApplicative.apply[Int, String](List(_.toString))(
//      List(10, 20))
//    val prod =
//      Applicative.listApplicative.product[Option](Applicative.optionApplicative)
//    val comp =
//      Applicative.listApplicative.compose[Option](Applicative.optionApplicative)
//    println(prod.map2(prod.unit(1), prod.unit(10))((a, b) => a + b))
//    println(comp.map2(comp.unit(1), comp.unit(10))((a, b) => a + b))
//    println("-----")
//    println(s"reverse: ${Traverse.listTraverse.reverse[Int](List(1, 2, 3, 4, 5))}")
//    println("-----")
//    println(Traverse.listTraverse.foldLeft(List(1, 2, 3, 4, 5))(0)(_ + _))
//    println(
//      Traverse.listTraverse.fuse[Option, Stream, Int, Int](List(1, 2, 3, 4, 5))(
//        a => Some(a),
//        a => Stream.apply(a))(Applicative.optionApplicative, Applicative.streamApplicative))
//    implicit val ml = AMonad.listMonad
//    val ot = OptionT[List, Int](List(Some(1), Some(2), None, Some(4)))
//
//    println(ot.flatMap(i => OptionT(List[Option[Int]](None))))
//    val eitherMonad = AMonad.eitherMonad[String]
//    val u = eitherMonad.unit(1).flatMap(_ => eitherMonad.unit(true))
//    val app = Applicative.monoidApplicative[List[Int]](Monoid.listMonoid)
//    val u2 = app.unit(1)
  }

}
