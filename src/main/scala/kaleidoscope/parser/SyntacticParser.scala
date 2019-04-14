package kaleidoscope.parser

import cats.Eval
import parseback._
import parseback.compat.cats._

import scala.util.matching.Regex

object SyntacticParser {

  implicit class SkipStartingSpacesHelper(val sc: StringContext) extends AnyVal {
    def ss(args: Any*): Regex = {
      val h = sc.parts.head
      s"""\\s*$h""".r
    }
  }

  lazy val doubleNum = ss"""\d+\.\d+"""
  lazy val plus = ss"""\+"""
  lazy val minus = ss"""\-"""
  lazy val multiply = ss"""\*"""
  lazy val divide = ss"""\/"""
  lazy val openingBracket = ss"""\("""
  lazy val closingBracket = ss"""\)"""
  lazy val identifier = ss"""[_a-zA-Z]+\w*"""
  lazy val defR = ss"""def"""
  lazy val extern = ss"""extern"""
  lazy val comma = ss""","""
  lazy val separator = ss""";"""

  lazy val top: Parser[List[FuncAST]] = (
    function ~ separator ~ top ^^ { (loc, f, _, funcs) => f::funcs }
      | function ^^ { (loc, f) => f::Nil }
    )

  lazy val function: Parser[FuncAST] = (
    defR ~ proto ~ expr ^^ { (loc, _, p, b) => InternalFuncAST(loc, p, BodyAST(loc, b)) }
      | extern ~ proto ^^ { (loc, _, p) => ExternalFuncAST(loc,p) }
    )

  lazy val proto: Parser[ProtoAST] =
    identifier ~ openingBracket ~ argsProto ~ closingBracket ^^ {
      (loc, i, _, a, _) => ProtoAST(loc, i.trim, a)
    }

  lazy val argsProto: Parser[ArgsProtoAST] = (
    argsProto ~ comma ~ identifier ^^ {
      (loc, a, _, i) => ArgsProtoAST(loc, i, Some(a))
    }
      | identifier ^^ {
      (loc, a) => ArgsProtoAST(loc, a, None)
    }
    )

  lazy val actionArgs: Parser[ActionArgsAST] = (
    actionArgs ~ comma ~ factor ^^ {
      (loc, a, _, e) => ActionArgsAST(loc, e, Some(a))
    }
      | factor ^^ {
      (loc, e) => ActionArgsAST(loc, e, None)
    }
    )

  lazy val expr: Parser[ExprAST] = (
    expr ~ plus ~ term ^^ {
      (loc, e, _, t) => AddAST(loc, e, t)
    }
      | expr ~ minus ~ term ^^ {
      (loc, e, _, t) => SubAST(loc, e, t)
    }
      | term
    )

  lazy val term: Parser[ExprAST] = (
    expr ~ multiply ~ factor ^^ { (loc, e, _, f) => MulAST(loc, e, f) }
      |
      factor
    )

  lazy val factor: Parser[ExprAST] = (
    identifier ~ openingBracket ~ actionArgs ~ closingBracket ^^ { (loc, i, _, a, _) => ActionAST(loc, i.trim, a) }
      |
      openingBracket ~> expr <~ closingBracket
      |
      doubleNum ^^ { (loc, str) => NumberAST(loc, str.trim.toDouble) }
      |
      identifier ^^ { (loc, str) => VariableAST(loc, str.trim) }
    )

  def parse(s: String) = top(
    LineStream[Eval](s)
  ).value
}

sealed trait ExprAST

final case class BodyAST(loc: List[Line], body: ExprAST) extends ExprAST {
  override def toString: String = body.toString
}

sealed trait FuncAST extends  ExprAST{
  def proto: ProtoAST
}

sealed trait BinaryAST extends ExprAST{
  def left: ExprAST
  def right: ExprAST
}

final case class InternalFuncAST(loc: List[Line], proto: ProtoAST, body: BodyAST) extends FuncAST {
  override def toString: String = s"internal $proto $body"
}

final case class ExternalFuncAST(loc: List[Line], proto: ProtoAST) extends FuncAST {
  override def toString: String = s"external $proto"
}

final case class ProtoAST(loc: List[Line], name: String, args: ArgsProtoAST) extends ExprAST {
  override def toString: String = s"$name($args)"
}

final case class ArgsProtoAST(loc: List[Line], name: String, nextArg: Option[ArgsProtoAST]) extends ExprAST {
  override def toString: String = nextArg match {
    case None => name
    case Some(a) => s"$name,$a"
  }
}

final case class ActionArgsAST(loc: List[Line], expr: ExprAST, nextArg: Option[ActionArgsAST]) extends ExprAST {
  override def toString: String = nextArg match {
    case None => expr.toString
    case Some(a) => s"$expr, $a"
  }
}

final case class ActionAST(loc: List[Line], name: String, args: ActionArgsAST) extends ExprAST {
  override def toString: String = s"$name($args)"
}

final case class MulAST(loc: List[Line], left: ExprAST, right: ExprAST) extends BinaryAST {
  override def toString: String = s"* $left $right"
}

final case class AddAST(loc: List[Line], left: ExprAST, right: ExprAST) extends BinaryAST {
  override def toString: String = s"+ $left $right"
}

final case class SubAST(loc: List[Line], left: ExprAST, right: ExprAST) extends BinaryAST {
  override def toString: String = s"- $left $right"
}

final case class NumberAST(loc: List[Line], value: Double) extends ExprAST {
  override def toString: String = value.toString
}

final case class VariableAST(loc: List[Line], name: String) extends ExprAST {
  override def toString: String = name
}

