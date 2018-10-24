package fpinscala.parallelism

import java.util.concurrent._

import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  //  val f: ExecutorService => Future[Any] = (e: ExecutorService) => ???

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a.apply(s)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that
  // just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled.
  // Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = {
//    println(s"[${Thread.currentThread().getName}] unit")
    (_: ExecutorService) =>
      UnitFuture(a)
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => {
      lazyUnit(f(a))
    }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having
  // `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we
  // want the evaluation of `f` to occur in a separate thread.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
//    println(s"[${Thread.currentThread().getName}] map2")
    (es: ExecutorService) =>
      {
//        println(s"[${Thread.currentThread().getName}] Running map2")
        val af = a(es)
        val bf = b(es)
        // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This
        // means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It
        // simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`
        // , applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future`
        // implementation that records the amount of time spent evaluating `af`, then subtracts that time from the
        // available time allocated for evaluating `bf`.
        UnitFuture(f(af.get, bf.get))
      }
  }

  //}

  //def asyncF[A, B](f: A => B): A => Par[B] =
  //a => lazyUnit(f(a))
  //
  def map2_2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(
      f: (A, B, C) => D): Par[D] = {
    val ab = map2[A, B, (A, B)](a, b)((a, b) => (a, b))
    map2[(A, B), C, D](ab, c)((ab, c) => f(ab._1, ab._2, c))
  }

  def map4[A, B, C, D, E](
      a: Par[A],
      b: Par[B],
      c: Par[C],
      d: Par[D]
  )(f: (A, B, C, D) => E): Par[E] = {
    val abc = map3[A, B, C, (A, B, C)](a, b, c)((a, b, c) => (a, b, c))
    map2[(A, B, C), D, E](abc, d)((abc, d) => f(abc._1, abc._2, abc._3, d))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List.empty[A])) { (pa, pas) =>
      map2(pa, pas) { (a, as) =>
        a :: as
      }
    }
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFold[A, T](z: T, list: IndexedSeq[A])(f: (A, T) => T)(
      f2: (T, T) => T): Par[T] = {
    if (list.isEmpty) {
      Par.unit(z)
    } else if (list.length == 1) {
      Par.unit(f(list.head, z))
    } else {
      val (l, r) = list.splitAt(list.length / 2)
      Par.map2(Par.fork(parFold(z, l)(f)(f2)), Par.fork(parFold(z, r)(f)(f2))) {
        (t1, t2) =>
          f2(t1, t2)
      }
    }
  }

//  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
//    as.foldRight(unit(List.empty[A])) { (a, pas) =>
//      if (f(a)) {
//        fork(map2(unit(a), pas) { (a, as) => a :: as })
//      } else {
//        fork(map2(unit(a), pas) { (_, as) => as })
//      }
//    }
//  }

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the
  // outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our
  // thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential
  // parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious
  // problem with the implementation, and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] = {
//    println(
//      s"[${Thread.currentThread().getName}] fork: Creating a Par, aka a function")
    es =>
      {
//        println(s"[${Thread.currentThread().getName}] Submitting to es: $es")
        val f = es.submit(new Callable[A] {
          override def call: A = {
//            println(s"[${Thread.currentThread().getName}] Blocking to get an A")
            val aa = a(es).get
//            println(s"[${Thread.currentThread().getName}] Returning an A, $aa")
            aa
          }
        })
//        println(s"[${Thread.currentThread().getName}] Submitted to es: $es")
        f
      }
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

//  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
//    fork(sequence(ps.map(asyncF(f))))

//  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
//    ps.foldRight(lazyUnit(List.empty[A]))((pa, pas) =>
//      map2(pa, pas)((a, as) => a :: as))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pas = as.map(asyncF(a => if (f(a)) List(a) else List.empty))
    map(sequence(pas))(as => as.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get)
        t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val res = run(es)(n).get()
    run(es)(choices(res))
  }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A],
                                           ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    flatMap(pa)(choices)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = es => {
    val k = run(es)(a).get()
    run(es)(f(k))
  }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser[Boolean, A](cond)(b => if (b) t else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser[Int, A](n)(a => choices(a))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
  }

  //}

  /*
Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel. We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
   */
  case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C)
      extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined

    def isCancelled = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

}

object Examples {
  import Par._
  // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these
  // sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def sum2(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val lp = Par.fork(Par.lazyUnit(sum(l)))
      val rp = Par.fork(Par.lazyUnit(sum(r)))
      Par.map2(lp, rp)(_ + _) // Recursively sum both halves and add the results together.
    }
}
