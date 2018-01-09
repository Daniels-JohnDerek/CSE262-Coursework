/** 
    Scanner for Pascal Compiler project
    CSE 262 - Programming Languages
    Fall 2017
    J. Femister

    Version 1.0 - Initial release
*/

import scala.util.parsing.combinator._

trait PascalASTNode
trait PascalToken extends PascalASTNode

// Special Symbols
case object ASSIGN extends PascalToken
case object COLON extends PascalToken
case object COMMA extends PascalToken
case object DIVIDE extends PascalToken
case object DOTDOT extends PascalToken
case object EQUALS extends PascalToken
case object GREATERTHAN extends PascalToken
case object GREATERTHANOREQUALTO extends PascalToken
case object HAT extends PascalToken
case object LESSTHAN extends PascalToken
case object LESSTHANOREQUALTO extends PascalToken
case object LBRACKET extends PascalToken
case object LPAREN extends PascalToken
case object MINUS extends PascalToken
case object NOTEQUALTO extends PascalToken
case object PERIOD extends PascalToken
case object PLUS extends PascalToken
case object RBRACKET extends PascalToken
case object RPAREN extends PascalToken
case object SEMICOLON extends PascalToken
case object TIMES extends PascalToken


// Reserved words
case object AND extends PascalToken
case object ARRAY extends PascalToken
case object BEGIN extends PascalToken
case object CASE extends PascalToken
case object CONST extends PascalToken
case object DIV extends PascalToken
case object DO extends PascalToken
case object DOWNTO extends PascalToken
case object ELSE extends PascalToken
case object END extends PascalToken
case object FILE extends PascalToken
case object FORWARD extends PascalToken
case object FOR extends PascalToken
case object FUNCTION extends PascalToken
case object IF extends PascalToken
case object IN extends PascalToken
case object MOD extends PascalToken
case object NIL extends PascalToken
case object NOT extends PascalToken
case object OF extends PascalToken
case object OR extends PascalToken
case object PROCEDURE extends PascalToken
case object PROGRAMx extends PascalToken
case object RECORD extends PascalToken
case object REPEAT extends PascalToken
case object SETx extends PascalToken
case object THEN extends PascalToken
case object TO extends PascalToken
case object TYPE extends PascalToken
case object UNTIL extends PascalToken
case object VAR extends PascalToken
case object WHILE extends PascalToken

case object WRITELN extends PascalToken
case object WRITE extends PascalToken

case object COMMENT extends PascalToken

case class IDENTIFIER(value:String) extends PascalToken
case class NUMBER(value:String) extends PascalToken
case class STRINGLIT(value:String) extends PascalToken

trait PascalCompilationError extends Exception
case class PascalLexerError(msg: String) extends PascalCompilationError

object PascalLexer extends RegexParsers {

    override def skipWhitespace = true
    override val whiteSpace = """[ \t\r\n\f]+""".r

    def assign  = ":=" ^^ (_ => ASSIGN)
    def colon   = ":" ^^ (_ => COLON)
    def comma   = "," ^^ (_ => COMMA)
    def divide  = "/" ^^ (_ => DIVIDE)
    def dotdot  = ".." ^^ (_ => DOTDOT)
    def equals  = "=" ^^ (_ => EQUALS)
    def greaterthanorequalto = ">=" ^^ (_ => GREATERTHANOREQUALTO)
    def greaterthan          = ">" ^^ (_ => GREATERTHAN)
    def hat         = "^" ^^ (_ => HAT)
    def lessthan    = "<" ^^ (_ => LESSTHAN)
    def lessthanorequalto = "<=" ^^ (_ => LESSTHANOREQUALTO)
    def lbracket    = "[" ^^ (_ => LBRACKET)
    def lparen      = "(" ^^ (_ => LPAREN)
    def minus       = "-" ^^ (_ => MINUS)
    def notequalto  = "<>" ^^ (_ => NOTEQUALTO)
    def period      = "." ^^ (_ => PERIOD)
    def plus        = "+" ^^ (_ => PLUS)
    def rbracket    = "]" ^^ (_ => RBRACKET)
    def rparen      = ")" ^^ (_ => RPAREN)
    def semicolon   = ";" ^^ (_ => SEMICOLON)
    def times       = "*" ^^ (_ => TIMES)

    val reservedWords = Map(
        "and" -> AND, 
        "array" -> ARRAY, 
        "begin" -> BEGIN, 
        "case" -> CASE,
        "const" -> CONST,
        "div" -> DIV,
        "do" -> DO,
        "downto" -> DOWNTO,
        "else" -> ELSE,
        "end" -> END,
        "file" -> FILE,
        "forward" -> FORWARD,
        "for" -> FOR,
        "function" -> FUNCTION,
        "if" -> IF,
        "in" -> IN,
        "mod" -> MOD,
        "nil" -> NIL,
        "not" -> NOT,
        "of" -> OF,
        "or" -> OR,
        "procedure" -> PROCEDURE,
        "program" -> PROGRAMx,
        "record" -> RECORD,
        "repeat" -> REPEAT,
        "set" -> SETx,
        "then" -> THEN,
        "to" -> TO,
        "type" -> TYPE,
        "until" -> UNTIL,
        "var" -> VAR,
        "while" -> WHILE,
        "writeln" -> WRITELN,
        "write" -> WRITE
    )

    val debug = false

    def identifier: Parser[PascalToken] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
        
        case s:String if reservedWords contains s => { if (debug) println(s"RESERVED: $s"); reservedWords(s) } 
        case s:String => { if (debug) println(s"IDENTIFIER: $s"); IDENTIFIER(s) }
    }

    def number: Parser[NUMBER] = """[0-9]+""".r ^^ { 
        case s:String => NUMBER(s) 
    }

    def stringlit: Parser[STRINGLIT] = """'([^']|'')*'""".r ^^ { 
        case s:String => STRINGLIT(s) 
    }

    def bcomment: Parser[PascalToken] = """\{[^}]*}""".r ^^ { 
        case x => COMMENT
    }

    def pcomment: Parser[PascalToken] = """\(\*.+\*\)""".r ^^ { 
        case x => COMMENT
    }

    def tokens: Parser[List[PascalToken]] = {
        phrase(rep1(pcomment | assign | colon | comma | divide | dotdot | equals | notequalto | greaterthanorequalto | 
                    greaterthan | hat | lessthanorequalto | lessthan |
                    lbracket | lparen | minus | period | plus | rbracket | rparen | semicolon | 
                    times | number | identifier | stringlit | bcomment )) ^^ { 
                    rawTokens => rawTokens.filter( _ != COMMENT)
        }
    }

    def apply(code: String) = {
        parse(tokens, code) match {
            case NoSuccess(msg, next) => throw new PascalLexerError(msg)
            case Success(result, next) => result
        }
    }

}
