/** 
    Main Program for Pascal Compiler project
    CSE 262 - Programming Languages
    Fall 2017
    J. Femister

    Version 1.0 - Initial release
*/

import scala.io.Source

object PascalCompiler {
  def apply(code: String) = {
    val tokens = PascalLexer(code)
    println()
    println(tokens)
    println()
    val ast = PascalParser(tokens)
    println(ast)
    println()
    ast
  }
}

object CompilerDriver {
    def main(args: Array[String]):Unit = {
        val code = Source.fromFile(args(0)).getLines.toList.mkString("\n")
        val ast = PascalCompiler(code)
        val intrp = new PascalInterpreter(ast)
        intrp.interp
    }
}