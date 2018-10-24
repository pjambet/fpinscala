package fpinscala.parsing

import java.util.regex.Pattern

import fpinscala.testing._
import fpinscala.testing.Prop._

import language.higherKinds
import scala.util.matching.Regex
//import language.implicitConversions

trait Parsers[Parser[+ _]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def defaultSucceed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def succeed[A](a: A): Parser[A]

//  def orString(s1: String, s2: String): Parser[String]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List.empty[A])
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List.empty[A])

  def map[A, B](pa: Parser[A])(f: A => B): Parser[B] =
//    flatMap(pa)(a => succeed(f(a)))
    flatMap(pa)(f andThen succeed)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(
      f: (A, B) => C): Parser[C] =
//    product(pa, pb).map(f.tupled)
    flatMap(pa)(a => map(pb)(b => f(a, b)))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def eof: Parser[String] =
    "\\z".r label "unexpected trailer character"

  def whitespace: Parser[String] = "\\s*".r

  private def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] =
    doubleString.map(_.toDouble) label "double literal"

  def quoted: Parser[String] =
    string("\"") *> thru("\"").map(_.dropRight(1))
//    token("\"") *> regex("[^\"]*".r) <* token("\"")

  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  def escapedQuoted: Parser[String] =
    token(quoted label "string literal")

  def attempt[A](p: Parser[A]): Parser[A]

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  def surround[A](start: Parser[Any], end: Parser[Any])(
      p: => Parser[A]): Parser[A] =
    start *> p <* end

  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    sep1(p, p2) or succeed(List.empty[A])

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)
    def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
    def sep(separator: Parser[Any]): Parser[List[A]] = self.sep(p, separator)
    def label(msg: String): Parser[A] = self.label(msg)(p)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

    def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(s))(s) == Right(s))

//    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
//      forAll(inputs ** Gen.string) { case (input, msg) =>
//          run(label(msg)(p))(input) match {
//            case Left(e) => errorMessage(e) == msg
//            case _ => true
//          }
//      }
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String = {
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
  }
}

case class ParseError(
    stack: List[(Location, String)] = List.empty,
    otherFailures: List[ParseError] = List.empty
) {

  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption
}
