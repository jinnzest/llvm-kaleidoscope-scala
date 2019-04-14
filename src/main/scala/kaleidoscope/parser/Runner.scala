package kaleidoscope.parser

import java.io.{File, PrintWriter}

import com.typesafe.config.ConfigFactory
import parseback.util.Catenable

import scala.io.StdIn
import scala.sys.process.{ProcessLogger, _}


object Runner {
  val resultName = "result"
  val resultLLName = s"$resultName.ll"
  val buildDirName = "build"

  val conf = ConfigFactory.load()

  val llcPath = conf.getString("llc.path")

  def runAndMeasure[A](n: String)(f: => (A)): A = {
    val start = System.currentTimeMillis()
    val res = f
    val end = (System.currentTimeMillis() - start)
    println(s"Exec time for $n: $end millis")
    res
  }

  def compile() = (for {
    _ <- compileLL()
    _ <- compileS()
    _ <- compileO()
  } yield ()) match {
    case Right(_) =>
      println("Successfully compiled")
      System.exit(0)
    case Left(err) =>
      System.err.println(s"COMPILER ERROR: $err")
      System.exit(-1)
  }

  private def runCMD(cmd: String) = {
    val stderr = new StringBuilder
    cmd ! ProcessLogger(
      new StringBuilder append _, stderr append _
    ) match {
      case 0 => Right(())
      case _ => Left(stderr.toString)
    }
  }

  private def compileO() = runCMD(s"gcc $buildDirName/$resultName.o -o $buildDirName/$resultName")

  private def compileS() = runCMD(s"gcc -c $buildDirName/$resultName.s -o $buildDirName/$resultName.o")

  private def compileLL() = runCMD(s"$llcPath $buildDirName/$resultLLName")

  def persistIR(str: String) = {
    new File(buildDirName).mkdir()
    new PrintWriter(s"$buildDirName/$resultLLName") {
      write(str)
      close()
    }
  }

  def readInput(args: Array[String]) =
    if (args.nonEmpty) {
      args.head
    } else {
      println("Enter your program and press enter:")
      StdIn.readLine()
    }

  def main(args: Array[String]): Unit = {
    deleteBuildDir()
    tryCompileToBinary(kaleidoscopeToIR(readInput(args)))
  }

  def deleteBuildDir() = new File("build").delete()

  def kaleidoscopeToIR(input: String) = {
    runAndMeasure("compile") {
      SyntacticParser.parse(input)
    } match {
      case Left(l) => Left(l.map(_.toString))
      case Right(r) =>
        genASTGraph(input, r)
        validateAndGenerateIR(input, r)
    }
  }

  private def validateAndGenerateIR(input: String, r: Catenable[List[FuncAST]]) = {
    runAndMeasure("validate semantic and generate IR") {
      validateSemanticAndGenerateIR(input, r)
    }
  }

  private def genASTGraph(input: String, r: Catenable[List[FuncAST]]) = {
    runAndMeasure("gen graph") {
      GraphGenerator.generateGraph("ast.png", input, r.toList.head)
    }
  }

  private def tryCompileToBinary(result: Either[List[String], String]) =
    result match {
      case Left(errors) => handleErrors(errors)
      case Right(irStr) => compileToBinary(irStr)
    }

  private def compileToBinary(irStr: String) =
    runAndMeasure("compile IR") {
      persistIR(irStr)
      compile()
    }

  private def handleErrors(errors: List[String]) = {
    printErrors(errors)
    System.exit(-1)
  }

  private def validateSemanticAndGenerateIR(input: String, r: Catenable[List[FuncAST]]) = {
    val protos = parseASTFuncs(r)
    SemanticValidator.validate(protos) match {
      case Nil => generateIR(input, protos)
      case validationResult: List[String] => Left(validationResult)
    }
  }

  private def generateIR(input: String, protos: List[FunctionProto]) =
    runAndMeasure("parse semantic tree and generate IR") {
      Right(WrapperGenerator.generate(input, protos))
    }

  private def parseASTFuncs(r: Catenable[List[FuncAST]]) =
    r.toList.head.map(SemanticParser.parse)

  private def printErrors(validationResult: List[String]) = {
    Console.err.println(s"There are ${validationResult.length} ERROR(S):")
    validationResult.foreach(Console.err.println)
  }
}
