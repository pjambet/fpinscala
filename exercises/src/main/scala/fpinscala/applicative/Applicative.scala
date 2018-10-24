package fpinscala
package applicative

import monads.Functor
import state._

import scala.collection.mutable.ListBuffer
//import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {

  // primitive
  def unit[A](a: => A): F[A]

  // primitives unit/apply OR unit/map2

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
//    map2(fab, fa)(_(_))
    map2(fab, fa)((fab, a) => fab(a))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    //    apply(apply(unit(f.curried))(fa))(fb)
    apply(map(fa)(f.curried))(fb)
  def map3_map2[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
      f: (A, B, C) => D): F[D] =
    map2(map2(fa, fb)((a, b) => (a, b)), fc)((ab, c) => f(ab._1, ab._2, c))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
      f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List.empty[A]))((fa, fla) => map2(fa, fla)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def map2_product[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    map(product(fa, fb))(f.tupled)

  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
    p match { case (a, (b, c)) => ((a, b), c) }

  def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
    (i, i2) => (f(i), g(i2))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def product[G[_]](
      G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[A => B], G[A => B]))(
          fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }

  def compose[G[_]](
      G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(
          f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)((a, b) => f(a, b)))
    }

  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V]))((pair, accF) =>
      map2(pair._2, accF)((v, acc) => acc.updated(pair._1, v)))
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {

  // primitives: unit/flatMap OR unit/compose OR unit/map/join

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

//  override def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
//    for {
//      a <- fa
//      b <- fb
//      c <- fc
//    } yield f((a,b,c))

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

//  def map2_flatMap[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
//    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object AMonad {
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] =
    new Monad[({ type f[x] = Either[E, x] })#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](ma: Either[E, A])(
          f: A => Either[E, B]): Either[E, B] = ma match {
        case Left(e)  => Left(e)
        case Right(a) => f(a)
      }
    }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(
        f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = a :: List.empty

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F],
                           N: Monad[N],
                           T: Traverse[N]): Monad[({ type f[x] = F[N[x]] })#f] =
    new Monad[({ type f[x] = F[N[x]] })#f] {
      override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))
      override def flatMap[A, B](mna: F[N[A]])(f: A => F[N[B]]): F[N[B]] =
        F.flatMap(mna)(na => F.map(T.traverse(na)(f))(N.join))
    }

  case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
    def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] = {
      OptionT(M.flatMap(value) {
        case None    => M.unit(None)
        case Some(a) => f(a).value
      })
    }
  }

//  implicit def toMonadOps[F[_], A](m: Monad[F[A]]): MonadOps[F, A] =
//    new MonadOps[F, A](m)
//
//  class MonadOps[F[_], A](m: Monad[F[A]]) { self =>
//    def flatMap[B](fa: F[A])(f: A => F[B]): F[B] = m.flatMap(fa)(f)
//  }

}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A],
                               b: Stream[B])( // Combine elements pointwise
                                             f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  val listApplicative: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = a :: List.empty[A]

//    override def map2[A, B, C](fa: List[A], fb: List[B])(
//        f: (A, B) => C): List[C] =
//      fa.zip(fb).map(f.tupled)

    override def apply[A, B](fab: List[A => B])(fa: List[A]): List[B] = {
//      fab.zip(fa).map(t => t._1(t._2))
      var i = 0
      val bs = new ListBuffer[B]
      while (i < fab.length && i < fa.length) {
        bs += fab(i)(fa(i))
        i += 1
      }
      bs.result()
    }
  }

  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(
        f: (A, B) => C): Option[C] = (fa, fb) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _                  => None
    }
  }

  def validationApplicative[E]
    : Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
          f: (A, B) => C): Validation[E, C] = {
        (fa, fb) match {
          case (Failure(head, tail), Failure(head2, tail2)) =>
            Failure(head, tail ++ Vector(head2) ++ tail2)
          case (Success(_), f @ Failure(_, _)) => f
          case (f @ Failure(_, _), Success(_)) => f
          case (Success(a), Success(b))        => Success(f(a, b))
        }
      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](
      M: Monoid[M]): Applicative[({ type f[x] = Const[M, x] })#f] =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      //      override def apply[A, B](fab: Const[M, A => B])(fa: Const[M, A]): Const[M, B] = super.apply(fab)(fa)
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }

}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(
      implicit G: Applicative[G]): G[F[B]] =
    sequence[G, B](map(fa)(f))

  def sequence[G[_], A](fma: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = f(a)
  }

  val idApplicative: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = a
    override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] = fab(fa)
//    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idApplicative)

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](as)(f)(
      monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(AMonad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- set(s2)
      } yield b).run(s)

  def mapAccum2[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = {
    val state: State[S, F[B]] = traverseS(fa) { (a: A) =>
      get[S].flatMap { s1 =>
        val (b, s2) = f(a, s1)
        println(s"s1: $s1")
        println(s"s2: $s2")
        println(s"b: $b")
        set(s2).map { _ =>
          b
        }
      }
    }
    state.run(s)
  }

  override def toList[A](fa: F[A]): List[A] =
    mapAccum[List[A], A, Unit](fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum[Int, A, (A, Int)](fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = {
    val s = mapAccum2[List[A], A, A](fa, toList(fa).reverse) {
      (a: A, s: List[A]) =>
        // (A, List[A])
        println("HERE")
        println(s)
        (s.head, s.tail)
    }
    println(s)
    s._1
  }

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum[B, A, Unit](fa, z)((a, b) => ((), f(b, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil)     => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil)     => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    })._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    (mapAccum(fb, toList(fa)) {
      case (b, Nil)     => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    })._1

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(
      implicit G: Applicative[G],
      H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(a => (f(a), g(a)))(
      G product H)

  def compose[H[_]](
      implicit H: Traverse[H]): Traverse[({ type f[x] = F[H[x]] })#f] =
    new Traverse[({ type f[x] = F[H[x]] })#f] {
      override def traverse[G[_], A, B](fa: F[H[A]])(f: A => G[B])(
          implicit G: Applicative[G]): G[F[H[B]]] = {
        self.traverse(fa)(ha => H.traverse(ha)(f))
      }

      // alternative
//      override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = ???
//      override def sequence[G[_], A](fma: F[G[G[A]]])(implicit G: Applicative[G]): G[F[G[A]]] = ???

    }
}

object Traverse {
  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(
        implicit M: Applicative[G]): G[List[B]] =
      as.foldRight(M.unit(List.empty[B]))((a, acc) =>
        M.map2(f(a), acc)((b, bs) => b :: bs))

    override def sequence[G[_], A](as: List[G[A]])(
        implicit M: Applicative[G]): G[List[A]] =
      as.foldRight(M.unit(List.empty[A]))((ga, acc) =>
        M.map2(ga, acc)((a, as) => a :: as))

  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(
        implicit M: Applicative[G]): G[Option[B]] = fa match {
      case None    => M.unit(None)
      case Some(a) => M.map(f(a))(b => Some(b))
    }
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(
        implicit M: Applicative[G]): G[Tree[B]] =
      M.map2(f(fa.head), listTraverse.traverse(fa.tail)(a => traverse(a)(f)))(
        Tree(_, _))
  }

  // An example of a Foldable that is not a functor
  case class Iteration[A](a: A, f: A => A, n: Int) {
    def foldMap[B](g: A => B)(M: Monoid[B]): B = {
      def iterate(n: Int, b: B, c: A): B =
        if (n <= 0) b else iterate(n - 1, g(c), f(a))
      iterate(n, M.zero, a)
    }
  }
//  val f: Applicative[List]#f[Int] = Applicative.listApplicative#f
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
