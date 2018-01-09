/** 
    Parser and AST builder for Pascal Compiler project
    CSE 262 - Programming Languages
    Fall 2017
    Name: JohnDerek Daniels
    Email: JRD319
*/

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position, Positional, Reader, NoPosition}

case class Program(name:IDENTIFIER, io:List[IDENTIFIER], blk:Block) extends PascalASTNode
case class ConstDef(name:IDENTIFIER, value:Constant) extends PascalASTNode
case class VarDecl(name:List[IDENTIFIER], typ:PascalType) extends PascalASTNode
case class Block(cdlist:List[ConstDef],vdlist:List[VarDecl],stp:List[Statement]) extends PascalASTNode
case class Constant(sign:String,value:NUMBER) extends PascalASTNode 

class PascalType extends PascalASTNode

class SimpleType extends PascalType

case class NamedType(name:IDENTIFIER) extends PascalType

case class Relop(op:PascalASTNode, se1:PascalASTNode, se2:PascalASTNode) extends PascalASTNode
case class Binop(op:PascalASTNode, t1:PascalASTNode, t2:PascalASTNode) extends PascalASTNode
case class Unop(op:PascalASTNode, e:PascalASTNode) extends PascalASTNode

class Statement extends PascalASTNode
case class AssignmentStatement(lhs:PascalASTNode, rhs:PascalASTNode) extends Statement
case class CompoundStatement(list:List[Statement]) extends Statement
case class WhileStatement(cond:PascalASTNode, body:Statement) extends Statement
case class RepeatStatement(cond:PascalASTNode, body:Statement) extends Statement
case class IfStatement(cond:PascalASTNode, thenstat:Statement, elsestat:Statement) extends Statement
case class ForStatement(indexvar:IDENTIFIER, startexp:PascalASTNode, endexp:PascalASTNode, dir:PascalToken, statement:Statement) extends Statement 
case class OutputValue(e1:PascalASTNode, e2:Option[PascalASTNode], e3:Option[PascalASTNode]) extends PascalASTNode
case class WriteStatement(elist:List[OutputValue], eol:Boolean) extends Statement

class PascalTokenReader(tokens: Seq[PascalToken]) extends Reader[PascalToken] {
  override def first: PascalToken = { /* if (true) println(s"first: ${tokens.head}"); */ tokens.head }
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[PascalToken] = new PascalTokenReader(tokens.tail)
}

case class PascalParserError(msg: String) extends PascalCompilationError

object PascalParser extends Parsers {
    override type Elem = PascalToken
    val debug = true

    def identifier: Parser[IDENTIFIER] = {
        accept("identifier", { case id @ IDENTIFIER(name) => id })
    }

    def number: Parser[NUMBER] = {
        accept("number", { case n @ NUMBER(num) => n })
    }

    def sign: Parser[PascalToken] = PLUS | MINUS

    def constant: Parser[Constant] = opt( sign ) ~ number  ^^ {
        case Some(s) ~ n => Constant(s.toString, n) // HI 
        case None ~ n => Constant("", n)
    }

    def program: Parser[Program] = PROGRAMx ~ identifier ~ LPAREN ~ identifier_list ~ RPAREN ~ SEMICOLON ~ block ~ PERIOD ^^ {
        case _~id~_~idList~_~_~blk~_ => Program(id,idList,blk)
    }

    def block: Parser[Block] = constant_definition_part ~ variable_declaration_part ~ statement_part ^^ {
        case cdlist ~ vdlist ~ statement => Block(cdlist,vdlist,statement)
    }

    def statement_part: Parser[List[Statement]] = BEGIN ~> statement_sequence <~ END

    def statement_sequence: Parser[List[Statement]] = repsep(statement,SEMICOLON)   

    def statement: Parser[Statement] = structured_statement | simple_statement
    
    def simple_statement: Parser[Statement] = assignment_statement

    def assignment_statement: Parser[Statement] = variable ~ ASSIGN ~ expression ^^ {
        case lhs ~_~ rhs => AssignmentStatement(lhs,rhs)
    }

    def structured_statement: Parser[Statement] = conditional_statement | compound_statement | repetitive_statement | write_statement

    def compound_statement: Parser[Statement] = BEGIN ~> statement_sequence <~ END ^^ {
        case list => CompoundStatement(list)
    }

    def repetitive_statement: Parser[Statement] = while_statement | repeat_statement | for_statement

    def while_statement: Parser[Statement] = WHILE ~> expression ~ DO ~ statement ^^ {
        case cond ~_~ body => WhileStatement(cond,body)
    }

    def repeat_statement: Parser[Statement] = REPEAT ~> statement_sequence ~ UNTIL ~ expression ^^ {
        case cond ~_~ body => RepeatStatement(body,CompoundStatement(cond))
    }

    def conditional_statement: Parser[Statement] = if_statement

    def for_statement: Parser[Statement] = FOR ~> identifier ~ ASSIGN ~ expression ~ ( TO | DOWNTO ) ~ expression ~ DO ~ statement ^^ {
        case indexvar ~_~ strtexp ~ dir ~ endexp ~_~ statement => ForStatement(indexvar,strtexp,endexp,dir,statement)
    }

    def if_statement: Parser[Statement] = IF ~> expression ~ THEN ~ statement ~ opt(ELSE ~> statement) ^^ {
        case cond ~_~ thenstate ~ None => IfStatement(cond,thenstate,CompoundStatement(List[Statement]()))
        case cond ~_~ thenstate ~ Some(elsestate) => IfStatement(cond,thenstate,elsestate)
    }

    def output_value: Parser[OutputValue] = expression ~ opt( COLON ~> expression ) ~ opt( COLON ~> expression ) ^^ {
        case e1 ~ e2 ~ e3 => OutputValue(e1,e2,e3)
    }

    def write_statement: Parser[Statement] = (WRITELN | WRITE) ~ LPAREN ~ repsep( output_value, COMMA ) ~ RPAREN ^^ {
        case eol ~ _ ~ elist ~ _ => {
            WriteStatement(elist, eol == WRITELN)
        }
    }

    def expression: Parser[PascalASTNode] = simple_expression ~ opt(relational_operator ~ simple_expression) ^^ {
        case se1 ~ None => se1
        case se1 ~ Some(op ~ se2) => Relop(op,se1,se2)
    }

    def relational_operator: Parser[PascalASTNode] = EQUALS | NOTEQUALTO | LESSTHAN | LESSTHANOREQUALTO | GREATERTHAN | 
                                            GREATERTHANOREQUALTO | IN

    def simple_expression: Parser[PascalASTNode] = opt(sign) ~ term ~ rep(addition_operator ~ term) ^^ {
        case None ~ term ~ addopp => { addopp.foldLeft(term) { case (term1, opp ~ term2) => Binop(opp,term1,term2)} }
        case Some(s) ~ term ~ addopp => { addopp.foldLeft(term) { case (term1, opp ~ term2) => Binop(opp,term1,term2)} }
    }

    def addition_operator: Parser[PascalASTNode] = PLUS | MINUS | OR 

    def multiplication_operator: Parser[PascalASTNode] = TIMES | DIVIDE | DIV | MOD | AND 

    def term: Parser[PascalASTNode] = factor ~ rep(multiplication_operator ~ factor) ^^ {
        case factor1 ~ factor2 => { factor2.foldLeft(factor1) { case (factor1, multop ~ factor2) => Binop(multop, factor1, factor2)} }
    }

    def factor: Parser[PascalASTNode] = LPAREN ~> expression <~ RPAREN | number | variable
    
    def variable: Parser[PascalASTNode] = identifier

    def constant_definition_part: Parser[List[ConstDef]] = CONST ~> repsep(constant_definition, SEMICOLON)

    def constant_definition: Parser[ConstDef] = identifier ~ EQUALS ~ constant ^^ {
        case name ~_~ value => ConstDef(name,value)
    }

    def variable_declaration_part: Parser[List[VarDecl]] = VAR ~> repsep(variable_declaration, SEMICOLON)

    def variable_declaration: Parser[VarDecl] = identifier_list ~ COLON ~ atype ^^ {
        case name ~_~ typ => VarDecl(name,typ)
    }

    def identifier_list: Parser[List[IDENTIFIER]] = repsep(identifier, COMMA )

    def atype: Parser[PascalType] = identifier ^^ {
        case name => NamedType(name) 
    }

    def apply(tokens: Seq[PascalToken]) = {
        val reader = new PascalTokenReader(tokens)
        program(reader) match {
            case NoSuccess(msg, next) => throw new PascalParserError(msg)
            case Success(result, next) => result
        }
    }

}