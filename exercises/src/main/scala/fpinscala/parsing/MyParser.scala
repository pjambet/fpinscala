package fpinscala.parsing

import fpinscala.parsing.MyParserTypes.{Failure, Parser, Success}

import scala.util.matching.Regex

object MyParserTypes {

  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A] {

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _             => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _                => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _             => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(get, charsConsumed) => Success(get, n + charsConsumed)
      case _                           => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean)
      extends Result[Nothing]

  def firstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length) -1
    else s1.length - offset
  }
}

object MyParser extends Parsers[Parser] {

  import MyParserTypes._

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    val loc = Location(input, 0)
    p(loc) match {
      case Success(get, _) => Right(get)
      case Failure(get, _) =>
        println(get.stack.map { s => println(s._1.line, s._1.col)})
        Left(get)
    }
  }

  override def attempt[A](p: Parser[A]): Parser[A] =
    loc => {
      p(loc).uncommit
    }

  override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  override def string(s: String): Parser[String] = {
    val msg = s"'$s'"
    location: Location =>
      val i = firstNonMatchingIndex(location.input, s, location.offset)
      if (i == -1) {
        Success(s, s.length)
      } else {
        Failure(location.advanceBy(i).toError(msg), i != 0)
      }
  }

  override def slice[A](p: Parser[A]): Parser[String] =
    (location: Location) =>
      p(location) match {
        case Success(get, charsConsumed) =>
          Success(location.input.substring(location.offset,
                                           location.offset + charsConsumed),
                  charsConsumed)
        case f @ Failure(_, _) => f
    }

  override def regex(r: Regex): Parser[String] =
    (location: Location) =>
      r.findPrefixOf(location.input.substring(location.offset)) match {
        case Some(m) => Success(m, m.length)
        case None    => Failure(location.toError(s"Failed to match $r"), false)
    }

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    l => {
      p(l).mapError(_.push(l, msg))
    }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    l => {
      p(l).mapError(_.label(msg))
    }

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
    l => {
      s1(l) match {
        case Failure(_, false) => s2(l)
        case r                 => r
      }
    }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    loc =>
      p(loc) match {
        case Success(a, n) =>
          f(a)(loc.advanceBy(n))
            .addCommit(n != 0)
            .advanceSuccess(n)
        case f @ Failure(_, _) => f
    }
}
