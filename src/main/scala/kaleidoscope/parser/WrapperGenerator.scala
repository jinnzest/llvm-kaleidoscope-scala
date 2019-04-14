package kaleidoscope.parser

/**
  * The class generates LLVM IR based main function, code reading input variable/variables, kaleidoscope source code printing
  * and printing results of running top hierarchy method run defined by user in kaleidoscope source code
  */
object WrapperGenerator {

  def escapedLen(str: String) = str.replace("""\0""", "").length
  val enterVariableStr = """Enter variable %s: \00"""
  val enterVariableLen = escapedLen(enterVariableStr)
  val scanfPattern = """%lf\00"""
  val scanfLen = escapedLen(scanfPattern)
  val sourceCodeStr = """Source code: %s\0A\00"""
  val sourceCodeLen = escapedLen(sourceCodeStr)
  val resultStr = """Result: %lf\0A\00"""
  val resultLen = escapedLen(resultStr)

  implicit class SkipStartingSpacesHelper(val sc: StringContext) extends AnyVal {
    def rawSS(args: Any*): String = {
      StringContext(sc.parts: _*).standardInterpolator(s => s, args).stripMargin.replaceAll("\n", "\n\t")
    }
  }

  def generateReadingInputVariables(args: List[String]) =
    args.foldLeft(";reading input variables") { (acc, a) =>
      val argLen = a.length + 1
      acc +
        rawSS"""
              |%var.$a.alloc.str = alloca i8*, align 8
              |store i8* getelementptr inbounds ([$argLen x i8], [$argLen x i8]* @.str.$a, i32 0, i32 0), i8** %var.$a.alloc.str, align 8
              |%var.$a.load.str = load i8*, i8** %var.$a.alloc.str, align 8
              |%var.$a.print = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([$enterVariableLen x i8], [$enterVariableLen x i8]* @.str.variable_title, i32 0, i32 0), i8* %var.$a.load.str)
              |%var.$a.alloc.dbl = alloca double, align 8
              |%var.$a.scanf = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([$scanfLen x i8], [$scanfLen x i8]* @.str.scanf_pattern, i32 0, i32 0), double* %var.$a.alloc.dbl)
              |%var.$a = load double, double* %var.$a.alloc.dbl, align 8
              |"""
    }

  def printSourceCode(inputLen: Int) = {
    val len = inputLen + 1
    rawSS"""
          |;print source code
          |%print.source.alloc = alloca i8*, align 8
          |store i8* getelementptr inbounds ([$len x i8], [$len x i8]* @.str.code, i32 0, i32 0), i8** %print.source.alloc, align 8
          |%print.source.load = load i8*, i8** %print.source.alloc, align 8
          |%print.source.call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([$sourceCodeLen x i8], [$sourceCodeLen x i8]* @.str.code_title, i32 0, i32 0), i8* %print.source.load)
          |
      |"""
  }

  def initialization(code: String, args: List[String]) = {
    val argNames = args.foldLeft("") { (acc, v) =>
      raw"""$acc@.str.$v =  unnamed_addr constant [${v.length + 1} x i8] c"$v\00", align 1
           |""".stripMargin
    }
    val codeLen = code.length + 1
    rawSS""";ModuleID = 'result.ir'
         |target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
         |@.str.variable_title = unnamed_addr constant [$enterVariableLen x i8] c"$enterVariableStr", align 1
         |@.str.scanf_pattern = unnamed_addr constant [$scanfLen x i8] c"$scanfPattern", align 1
         |@.str.result_title = unnamed_addr constant [$resultLen x i8] c"$resultStr", align 1
         |@.str.code = unnamed_addr constant [$codeLen x i8] c"$code\00", align 1
         |@.str.code_title = unnamed_addr constant [$sourceCodeLen x i8] c"$sourceCodeStr", align 1
         |
      |$argNames
         |declare i32 @printf(i8*, ...)
         |
         |declare i32 @scanf(i8*, ...)
         |
         |""".stripMargin
  }

  def generateIR(args: List[String], functions: String, input: String) =
    initialization(input, args) + s";start of llvm code generated from kaleidoscope code\n $functions\n;end of llvm code generated from kaleidoscope code\n" + generateMain(args, input.length)

  val generatePrintResult =
    rawSS"""%call.result = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([$resultLen x i8], [$resultLen x i8]* @.str.result_title, i32 0, i32 0), double %run.result)
          |ret void
          |"""

  def generateCallRun(args: List[String]) = {
    val argsStr = args.map(a => s"double %var.$a").mkString(", ")
    rawSS"""
          |;calling run function
          |%run.result = call double @run($argsStr)
          |"""
  }

  def generateMain(args: List[String], inputLen: Int) = {
    "\ndefine void @main()  {\n" +
      printSourceCode(inputLen) +
      generateReadingInputVariables(args) +
      generateCallRun(args) +
      generatePrintResult +
      "\n}"
  }

  def generate(input: String, funcs: List[FunctionProto]) = {
    val args = funcs.filter(_.name == "run").head.args
    val generatedFuncs = funcs.map(Generator.codegenExpr).map(_.statement).mkString("\n")
    generateIR(args, generatedFuncs, input)
  }
}
