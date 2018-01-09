/* 
    Interpreter for Pascal Compiler project
    CSE 262 - Programming Languages
    Fall 2017
    J. Femister

    Version 1.0 - Initial release
*/


import scala.collection.mutable.Map

object SymbolTable {
    val table = Map[String,FlexNumber]()
    def add(name:String, value:FlexNumber) = { /* println(s"add: name=$name, value=$value"); */ table += (name ->value) }
}

class PascalInterpreter(ast:PascalASTNode) {
    val debug = false
    def interp = {
        def block(blk:Block) = {
            def const_def(cd:ConstDef) = {
                cd match {
                    case ConstDef(IDENTIFIER(name), Constant(s,NUMBER(n))) => {
                        SymbolTable.add(name, FlexNumber(n))
                    }
                }
            }

            def var_decl(vd:VarDecl) = {
                vd match {
                    case VarDecl(vlist, typ) => vlist.foreach( (v) => { v match { case IDENTIFIER(i) => SymbolTable.add(i, FlexNumber("0")) } } ) 
                }
            }

            def expression(exp:PascalASTNode):FlexNumber = {
                val eval = exp match {
                    case Relop(op, se1, se2) => {
                        val se1val = expression(se1)
                        val se2val = expression(se2)
                        if (debug) println(s"RELOP: se1val= $se1val, se2val=$se2val, op=$op")
                        op match {
                            case EQUALS => se1val == se2val
                            case GREATERTHAN => se1val > se2val
                            case GREATERTHANOREQUALTO => se1val >= se2val
                            case LESSTHAN => se1val < se2val
                            case LESSTHANOREQUALTO => se1val <= se2val
                            case NOTEQUALTO => se1val != se2val
                        } 
                    }
                    case Binop(op, x1, x2) => {
                        val v1 = expression(x1)
                        val v2 = expression(x2)
                        op match {
                            case PLUS => v1 + v2
                            case MINUS => v1 - v2
                            case TIMES => v1 * v2
                            case DIVIDE => v1 / v2 
                            case DIV => v1 / v2
                            case MOD => v1 % v2
                            case AND => v1 & v2
                            case OR => v1 | v2
                        }
                    }
                    case NUMBER(n) => new FlexNumber(n) 
                    case IDENTIFIER(id) => SymbolTable.table(id)
                }
                if (debug) println(s"interp:expression: eval=$eval")
                eval
            }

            def statement(st:Statement):Unit = {
                if (debug) println(s"interp.statement: st=$st")
                st match {
                    case AssignmentStatement(lhs, rhs) => {
                        val e = expression(rhs)
                        val IDENTIFIER(varname) = lhs
                        SymbolTable.add(varname,e)
                        //println(e)
                    }

                    case CompoundStatement(statementlist) => {
                        statementlist.foreach(statement(_))
                    }

                    case WhileStatement(condition, body) => {
                        val TRUE = 1
                        while (expression(condition).ival == TRUE) statement(body)
                    }

                    case RepeatStatement(condition, body) => {
                        val TRUE = 1
                        do {
                            statement(body)
                        } while (expression(condition).ival != TRUE);
                    }

                    case IfStatement(condition, thenstat, elsestat) => {
                        val TRUE = 1
                        if (expression(condition).ival == TRUE) 
                            statement(thenstat)
                        else 
                            statement(elsestat)
                    }

                    case ForStatement(index, exp1, exp2, dir, stat) => {
                        val startval = expression(exp1)
                        val endval = expression(exp2)
                        var indexvar = startval
                        val IDENTIFIER(varname) = index
                        SymbolTable.add(varname,indexvar)
                        val one = FlexNumber(1)
                        while ((indexvar <= endval).toBoolean) {
                            statement(stat)
                            if (dir == TO)
                                indexvar = indexvar + one
                            else
                                indexvar = indexvar - one
                            SymbolTable.add(varname,indexvar)    
                        }
                    }

                    case WriteStatement(elist, eol) => {
                        elist.foreach( (exp:OutputValue) => {
                            var fmt = exp match {
                                case OutputValue(e1, None, None) => {
                                    val v1 = expression(e1)
                                    printf("%d", v1.ival)
                                } 
                                case OutputValue(e1, Some(e2), None) => {
                                    val v1 = expression(e1)
                                    val v2 = expression(e2)
                                    printf("%" + v2.ival + "d", v1.ival)

                                } 
                                case OutputValue(e1, Some(e2), Some(e3)) => {
                                    val v1 = expression(e1)
                                    val v2 = expression(e2)
                                    printf("%" + v2.ival + "d", v1.ival)
                                }
                            }
                        })
                        if (eol) println()
                    }
                }
            }

            blk match {
                case Block(cdlist,vdlist,statementlist) => {
                    cdlist.foreach(const_def(_))
                    vdlist.foreach(var_decl(_))
                    statementlist.foreach(statement(_))
                }
            }
        }

        ast match { case Program(IDENTIFIER(name), io, blk) => block(blk) }
    }
    
}