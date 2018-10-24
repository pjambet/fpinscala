package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

import language.higherKinds
import scala.annotation.tailrec

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map[(A, B), A](fab)(_._1), map[(A, B), B](fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa)  => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  // unit / flatmap
  def map_flatMap[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def compose_flatMap[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    (a: A) => flatMap(f(a))(g)
  def join_flatMap[A, B](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)

  // unit / compose
  def flatMap_compose[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose[Unit, A, B](_ => ma, f)(())
  def map_compose[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap_compose(ma)(a => unit(f(a)))
  def join_compose[A, B](mma: M[M[A]]): M[A] =
    flatMap_compose(mma)(identity)

  // unit / map / join
  def flatMap_join[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))
  def compose_join[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    (a: A) => join(map(f(a))(g))

  override def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List.empty[A]))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def _replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if (n <= 0) unit(List.empty[A])
    else map2(ma, _replicateM(n - 1, ma))(_ :: _)

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    (a: A) => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    compose[Unit, A, B]((_: Unit) => ma, f)(())
  }

  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldLeft(unit(List.empty[A])) { (acc, a) =>
      map2(acc, f(a))((as, b) => if (b) a :: as else as)
    }

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join[B](map(ma)(f))

  def __compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    (a: A) => join[C](map(f(a))(g))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      ma flatMap f
  }

  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = a :: List.empty

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  class StateMonads[S] {
    type StateS[A] = State[S, A]

    val monad = new Monad[StateS] {
      override def unit[A](a: => A): StateS[A] = State(s => (a, s))

      override def flatMap[A, B](ma: StateS[A])(f: A => StateS[B]): StateS[B] =
        ma flatMap f
    }
  }

  def stateMonad[S]: Monad[({ type lambda[x] = State[S, x] })#lambda] =
    new Monad[({ type lambda[x] = State[S, x] })#lambda] {
      override def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](ma: State[S, A])(
          f: A => State[S, B]): State[S, B] = ma flatMap f
    }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] =
      ma.flatMap(f)
  }

  def readerMonad[R]: Monad[({ type f[x] = Reader[R, x] })#f] =
    new Monad[({ type f[x] = Reader[R, x] })#f] {
      def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
      override def flatMap[A, B](st: Reader[R, A])(
          f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(st.run(r)).run(r))
    }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def ask[R]: Reader[R, R] = Reader(r => r)
}
