package kaleidoscope.parser

case class StatementsAndVars(statement: String, varName: String)

object Generator {

  private var variablesSuffix = 1

  private def nextVarSuffix() = {
    variablesSuffix += 1
    variablesSuffix
  }

  def codegenInternalFunc(func: FunctionDefinition): StatementsAndVars = {
    val body = codegenExpr(func.body)
    val argsStr = func.args.map(a => s"double %$a").mkString(", ")
    val protoStr = s"define double @${func.name} ($argsStr)"
    if (body.varName.isEmpty) StatementsAndVars(s"$protoStr {\n\tret double ${body.statement}\n}\n", "")
    else StatementsAndVars(s"$protoStr {\n${body.statement}\n\tret double ${body.varName}\n}\n", "")
  }

  def codegenExternalFunc(func: FunctionExport): StatementsAndVars = {
    val argsStr = func.args.map(a => s"double %$a").mkString(", ")
    StatementsAndVars(s"declare double @${func.name}($argsStr)", "")
  }

  private def join(acc: String, v: String) = if (acc.isEmpty) v else s"$acc\n$v"

  def codegenFuncAction(func: FunctionAction): StatementsAndVars = {
    def extractParam(acc: StatementsAndVars, v: Semantic) = {
      val strAndTmpRes = codegenExpr(v)
      val params = s"double ${strAndTmpRes.varName}"
      val accParams = if (acc.varName.isEmpty) params else s"${acc.varName}, $params"
      StatementsAndVars(join(acc.statement, strAndTmpRes.statement), accParams)
    }

    val extractedParams = func.args.foldLeft(StatementsAndVars("", ""))(extractParam)
    val tmpVar = s"%func.${func.name}.tmp.result.${nextVarSuffix()}"
    StatementsAndVars(join(extractedParams.statement, s"\t$tmpVar = call double @${func.name}(${extractedParams.varName})"), tmpVar)
  }

  def codegenBinaryAction(action: BinaryAction): StatementsAndVars = {
    val leftResult = codegenExpr(action.left)
    val rightResult = codegenExpr(action.right)
    val tmpVar = s"%${action.name}.tmp.result.${nextVarSuffix()}"
    val statements = join(leftResult.statement, rightResult.statement)
    StatementsAndVars(join(statements, s"\t$tmpVar = f${action.name} double ${leftResult.varName}, ${rightResult.varName}"), tmpVar)
  }

  def codegenNum(n: Number) = StatementsAndVars("", n.value.toString)

  def codegenVar(s: Variable) = StatementsAndVars("", s"%${s.name}")

  def codegenExpr(e: Semantic): StatementsAndVars = e match {
    case call: BinaryAction => codegenBinaryAction(call)
    case call: FunctionAction => codegenFuncAction(call)
    case n: Number => codegenNum(n)
    case i: Variable => codegenVar(i)
    case fe: FunctionExport => codegenExternalFunc(fe)
    case fi: FunctionDefinition => codegenInternalFunc(fi)
    case unexpected => throw new InternalError(s"Unexpected: $unexpected")
  }
}
