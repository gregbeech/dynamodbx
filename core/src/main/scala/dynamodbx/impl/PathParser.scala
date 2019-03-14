package dynamodbx.impl

import dynamodbx._
import org.parboiled2.{CharPredicate, Parser, ParserInput}

trait BasicRules { this: Parser =>
  def wsp = rule { oneOrMore(anyOf(" \t\r\n")) }
  def owsp = rule { zeroOrMore(anyOf(" \t\r\n")) }
  def kw(s: String) = rule { ignoreCase(s) }
}

trait PathRules extends BasicRules { this: Parser =>
  def name = rule { capture((1 to 255).times(noneOf(":#.[], \t\r\n"))) ~> Name.apply _ }
  def nameSegment = rule { "." ~ name }
  def indexSegment = rule { "[" ~ capture(oneOrMore(CharPredicate.Digit)) ~ "]" ~> (s => ListIndex(s.toInt)) }
  def segment = rule { nameSegment | indexSegment }
  def path = rule { name ~ optional((1 to 32).times(segment)) ~> ((n, s) => s match {
    case Some(ss) => Path(n, ss.toList)
    case None => Path(n)
  }) }
}

class PathParser(val input: ParserInput) extends Parser with PathRules
