package kaleidoscope.parser

import java.io.File

import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.Factory.{graph, node}
import guru.nidi.graphviz.model.{Graph, Label, Node}

object GraphGenerator {

  var globalNum = 0

  def nextGlobalNum() = {
    globalNum += 1
    globalNum.toString
  }

  def nextNode(s: String) = node(nextGlobalNum).`with`(Label.of(s))

  def genNum(n: Double): List[Node] = nextNode(s"num: $n") :: Nil

  def genVar(v: String): List[Node] = nextNode(s"var: $v") :: Nil

  def genOp(name: String, l: ExprAST, r: ExprAST): List[Node] =
    nextNode(name).link(genLink(l) ::: genLink(r): _*) :: Nil

  def genAdd(l: ExprAST, r: ExprAST): List[Node] = genOp("add", l, r)

  def genMul(l: ExprAST, r: ExprAST): List[Node] = genOp("mul", l, r)

  def genSub(l: ExprAST, r: ExprAST): List[Node] = genOp("sub", l, r)

  def genCall(name: String, args: ExprAST): List[Node] =
    nextNode(s"call $name").link(genLink(args): _*) :: Nil

  def genProto(name: String, args: ExprAST): List[Node] =
    nextNode("proto").link(nextNode(s"name: $name") :: genLink(args): _*) :: Nil

  def genArg(name: String, nextArg: Option[ExprAST]): List[Node] = {
    nextNode(s"arg name: $name") :: (nextArg match {
      case None => Nil
      case Some(arg) => genLink(arg)
    })
  }

  def genArgsProto(name: String, nextArg: Option[ExprAST]): List[Node] =
    nextNode("proto args").link(genArg(name, nextArg): _*) :: Nil

  def genCallArg(expr: ExprAST, nextArg: Option[ExprAST]): List[Node] =
    genLink(expr) ::: (nextArg match {
      case None => Nil
      case Some(arg) => genLink(arg)
    })

  def genArgsCall(expr: ExprAST, nextArg: Option[ExprAST]): List[Node] =
    nextNode("call args").link(genCallArg(expr, nextArg): _*) :: Nil

  def genBody(b: ExprAST): List[Node] =
    nextNode("body").link(genLink(b): _*) :: Nil

  def genFuncs(funcs: List[ExprAST]): List[Node] =
    nextNode("funcs").link(funcs.flatMap(f => genLink(f)): _*) :: Nil

  def genInternalFunc(proto: ProtoAST, body: BodyAST): List[Node] =
    nextNode("int func").link(genLink(body) ::: genLink(proto): _*) :: Nil


  def genExternalFunc(proto: ProtoAST): List[Node] =
    nextNode("ext func").link(genLink(proto): _*) :: Nil

  def genLink(e: ExprAST): List[Node] = e match {
    case NumberAST(_, n) => genNum(n)
    case VariableAST(_, i) => genVar(i)
    case AddAST(_, l, r) => genAdd(l, r)
    case MulAST(_, l, r) => genMul(l, r)
    case SubAST(_, l, r) => genSub(l, r)
    case ActionAST(_, l, r) => genCall(l, r)
    case ArgsProtoAST(_, name, nextArg) => genArgsProto(name, nextArg)
    case ActionArgsAST(_, expr, nextArg) => genArgsCall(expr, nextArg)
    case InternalFuncAST(_, proto, body) => genInternalFunc(proto, body)
    case ExternalFuncAST(_, proto) => genExternalFunc(proto)
    case ProtoAST(_, n, args) => genProto(n, args)
    case BodyAST(_, b) => genBody(b)
  }

  def generateGraph(name: String, expr: String, funcs: List[ExprAST]) = {
    val sources = funcs.flatMap(genLink)
    val g: Graph = graph(expr).directed().`with`(nextNode("top").link(sources: _*))
    Graphviz.fromGraph(g).width(4000).render(Format.PNG).toFile(new File(s"build/$name"))
  }
}

