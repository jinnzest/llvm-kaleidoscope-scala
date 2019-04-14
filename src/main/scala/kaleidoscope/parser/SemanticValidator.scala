package kaleidoscope.parser

object SemanticValidator {

  def extractProtos(funcs: List[FunctionProto]): Map[String, FunctionProto] =
    funcs.foldLeft(Map.empty[String, FunctionProto]) { (acc, v) =>
      acc + (v.name -> v)
    }

  def validate(funcs: List[FunctionProto]): List[String] = {
    val protos = extractProtos(funcs)
    funcs.flatMap {
      case f: FunctionDefinition => validate(f, protos)
      case _ => Nil
    } ::: checkRunFunctionExists(funcs)
  }

  def checkRunFunctionExists(funcs: List[FunctionProto]): List[String] = {
    val runFuncs = funcs.filter(_.name == "run")
    if (runFuncs.length > 1) "There should be not more than 1 run function" :: Nil
    else if (runFuncs.isEmpty) "Run function must be defined!" :: Nil else Nil
  }

  def extractUsedVariables(semantic: Semantic): Set[String] =
    semantic match {
      case action: Action => extractUsedVariables(action)
      case _ => Set.empty
    }


  private def extractUsedVariables(action: Action): Set[String] =
    action.args.foldLeft(Set.empty[String]) { (acc, s) =>
      s match {
        case v: Variable => acc + v.name
        case a: Action => acc ++ extractUsedVariables(a)
        case _ => acc
      }
    }

  def extractUsedFunctions(semantic: Semantic): List[FunctionAction] =
    semantic match {
      case action: Action => extractUsedFunctions(action)
      case _ => Nil
    }

  private def extractUsedFunctions(action: Action): List[FunctionAction] =
    action.args.foldLeft(List.empty[FunctionAction]) { (acc, s) =>
      (s match {
        case f: FunctionAction => f :: extractUsedFunctions(f)
        case a: Action => extractUsedFunctions(a)
        case _ => Nil
      }) ::: acc
    }

  def validate(func: FunctionDefinition, protos: Map[String, FunctionProto]) =
    validateVariables(func) ++ validateFunctions(func, protos)

  private def validateFunctions(func: FunctionDefinition, protos: Map[String, FunctionProto]) =
    validateFunctionCalls(func.name, extractUsedFunctions(func.body), protos)

  private def validateVariables(func: FunctionDefinition) =
    validateVariableScopes(extractUsedVariables(func.body), func)

  private def validateVariableScopes(vars: Set[String], func: FunctionDefinition) =
    vars.foldLeft(List.empty[String]) { (acc, v) =>
      if (func.args.contains(v)) acc else s"There is no variable '$v' in scope of function '${func.name}'" :: acc
    }

  private def validateFunctionCalls(name: String, funcs: List[FunctionAction], protos: Map[String, FunctionProto]) =
    funcs.foldLeft(List.empty[String]) { (acc, f) =>
      if (protos.contains(f.name)) validateFuncArgsAmount(protos, acc, f)
      else s"No function '${f.name}' is defined but is called inside function '$name'" :: acc
    }

  private def validateFuncArgsAmount(protos: Map[String, FunctionProto], acc: List[String], f: FunctionAction) = {
    val toCallFuncArgumentsAmount = f.args.length
    val protoArgumentsAmount = protos(f.name).args.length
    if (toCallFuncArgumentsAmount == protoArgumentsAmount) acc
    else s"Function '${f.name}' arguments amount mismatch: expected $protoArgumentsAmount but got $toCallFuncArgumentsAmount instead" :: acc
  }
}