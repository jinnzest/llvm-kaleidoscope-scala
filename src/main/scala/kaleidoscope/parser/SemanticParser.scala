package kaleidoscope.parser

import scala.annotation.tailrec

object SemanticParser {

  def extractProtoArgs(args: ArgsProtoAST): List[String] = extractParams(args, Nil)

  def parseExpr(e: ExprAST): Semantic =
    e match {
      case VariableAST(_, name) => Variable(name)
      case NumberAST(_, value) => Number(value)
      case op: BinaryAST => extractOp(op)
      case ActionAST(_, name, argsAST) => FunctionAction(name, extractCallsArgs(argsAST))
      case unexpected => throw new InternalError(s"Unexpected $unexpected")
    }

  def parseActionExpr(e: ExprAST): Semantic =
    e match {
      case BodyAST(_, body) => parseExpr(body)
      case unexpected => throw new InternalError(s"Unexpected $unexpected")
    }

  @tailrec
  private def extractParams(e: ArgsProtoAST, r: List[String]): List[String] =
    e.nextArg match {
      case Some(nxt: ArgsProtoAST) => extractParams(nxt, e.name :: r)
      case None => e.name :: r
    }

  @tailrec
  private def extractCallParams(e: ActionArgsAST, r: List[ExprAST]): List[ExprAST] =
    e.nextArg match {
      case Some(nxt: ActionArgsAST) => extractCallParams(nxt, e.expr :: r)
      case None => e.expr :: r
    }

  def extractCallsArgs(argsAST: ActionArgsAST): List[Semantic] = extractCallParams(argsAST, Nil).map(parseExpr)

  def extractOp(op: BinaryAST): Action =
    op match {
      case AddAST(_, l, r) => BinaryAction("add", parseExpr(l), parseExpr(r))
      case MulAST(_, l, r) => BinaryAction("mul", parseExpr(l), parseExpr(r))
      case SubAST(_, l, r) => BinaryAction("sub", parseExpr(l), parseExpr(r))
      case unexpected => throw new InternalError(s"Unexpected $unexpected")
    }

  def parse(e: ExprAST): FunctionProto =
    e match {
      case InternalFuncAST(_, proto, bodyAST) => FunctionDefinition(proto.name, extractProtoArgs(proto.args), parseActionExpr(bodyAST))
      case ExternalFuncAST(_, proto) => FunctionExport(proto.name, extractProtoArgs(proto.args))
      case unexpected => throw new InternalError(s"Unexpected $unexpected")
    }
}

sealed trait Semantic

sealed trait Action extends Semantic {
  def name: String

  def args: List[Semantic]
}

sealed trait FunctionProto extends Semantic {
  def name: String

  def args: List[String]
}

final case class FunctionDefinition(name: String, args: List[String], body: Semantic) extends FunctionProto{
  override def toString: String = s"define $name (${args.mkString(", ")} $body)"
}

final case class FunctionExport(name: String, args: List[String]) extends FunctionProto{
  override def toString: String = s"export $name (${args.mkString(", ")})"
}

final case class FunctionAction(name: String, args: List[Semantic]) extends Action{
  override def toString: String = s"call $name(${args.map(_.toString).mkString(", ")})"
}

final case class BinaryAction(name: String, left: Semantic, right: Semantic) extends Action {
  override def args: List[Semantic] = left :: right :: Nil

  override def toString: String = s"name $left $right"
}

final case class Variable(name: String) extends Semantic {
  override def toString: String = name
}

final case class Number(value: Double) extends Semantic {
  override def toString: String = value.toString
}
