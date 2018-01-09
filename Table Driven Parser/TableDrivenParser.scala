/**
JohnDerek Daniels
Jrd319
CSE 262
Programming Assignment 3
Description: Table Driven Parser in Scala
*/


object TableDrivenParser extends App {

    val scanner = new Scanner(args(0))

    def parse_error() = {
        println(s"Parser error!")
        System.exit(1)
    }

    //Creates parse_tab and prod_tab
    val parse_tab = Array.ofDim[Int](10,12)
    val prod_tab = new Array[List[String]](19)
    val table = io.Source.fromFile("calculatorparsetable.txt").getLines.toArray
    val prodtable = io.Source.fromFile("calculatorproductions.txt").getLines.toList
    var lenCount = 0
    var lenStr = ""
    var strInt = 0

    val nonTerminalToken = Set("program","stmt_list","stmt","expr","term_tail","term","fact_tail","factor","add_op","mult_op","epsilon","fact")

    //Populate parse_tab
    for( x <- 0 to 9){
        lenStr = table(x).mkString
        var lenStrFor = lenStr.split("\\s+")
        for(y <- 0 to 12){
            if(y != 0) {
                strInt = lenStrFor(y).toInt
                parse_tab(x)(y - 1) = strInt
            }
        }
    }

    //Populate prod_tab
    for( x <- 0 to 18){
        lenStr = prodtable(x).mkString
        var lenStrFor = lenStr.split("\\s+").map(_.trim).toList
        var lenStrFormat = lenStrFor.drop(3)
        prod_tab(x) = lenStrFormat
    }

    //Create Stack
    val parse_stack = scala.collection.mutable.ListBuffer[String]()

    val start_symbol = "program"
    parse_stack += start_symbol

    val digits = ('0' to '9').toSet
    val letters = ('A' to 'Z').toSet ++ ('a' to 'z').toSet

    //Calls scanner to get next token
    var currtok = scanner.nexttoken

    //Loop
    while(true) {
        
        println("______________________________________")
        println("")
        println("Parse_Stack: " + parse_stack)

        //Pops top off the stack
        var stackTopPos = (parse_stack.length - 1)
        var expected_sym = parse_stack(stackTopPos)
        parse_stack -= expected_sym


        var stackTopInt: Int = 0
         
        var currtokInt: Int = 0;

        //Takes token from scanner 
        currtok match {
            case Id(_) => currtokInt = 0
            case Number(_) => currtokInt = 1
            case Read => currtokInt = 2
            case Write => currtokInt = 3
            case Assign => currtokInt = 4
            case Lparen => currtokInt = 5
            case Rparen => currtokInt = 6
            case Plus => currtokInt = 7
            case Minus => currtokInt = 8
            case Times => currtokInt = 9
            case Div => currtokInt = 10
            case Eof => currtokInt = 11
        }

        //IF the token is a terminal token
        if (!nonTerminalToken.contains(expected_sym)){
            var expected_symTok: Token = Eof
            var y = ""

            //Convert token from top of the stack to token object
            expected_sym match {
                case "read" => expected_symTok = Read
                case "write" => expected_symTok = Write
                case "id" => expected_symTok = Id(y)
                case "number" => expected_symTok = Number(y)
                case ":=" => expected_symTok = Assign
                case "(" => expected_symTok = Lparen
                case ")" => expected_symTok = Rparen
                case "+" => expected_symTok = Plus
                case "-" => expected_symTok = Minus
                case "*" => expected_symTok = Times
                case "/" => expected_symTok = Div
                case "$$$" => expected_symTok = Eof
            }

            //Checks if terminal token is Eof and exit loop
            if (expected_symTok == Eof) {
                println("\nEND OF FILE")
                println("Parsed Successfully!")
                System.exit(0)
            }

            //Checks if terminal token is equal to token expected from scanner
            if (expected_symTok == currtok) {
                //Moves currTok to next token
                var currtokUn = scanner.nexttoken
                val blankTok = ""

                currtok = currtokUn match {
                    case Id(_) => Id(blankTok)
                    case Number(_) => Number(blankTok)
                    case _ => currtokUn
                }
            }
            //Else causes error
            else {
                parse_error()
            }
        }

        //If token is non-terminal token
        else {
            //Convert non-terminal token to position
            expected_sym match{
                case "program" => stackTopInt = 0
                case "stmt_list" => stackTopInt = 1
                case "stmt" => stackTopInt = 2
                case "expr" => stackTopInt = 3
                case "term_tail" => stackTopInt = 4 
                case "term" => stackTopInt = 5
                case "fact_tail" => stackTopInt = 6
                case "factor" => stackTopInt = 7
                case "fact" => stackTopInt = 7
                case "add_op" => stackTopInt = 8
                case "mult_op" => stackTopInt = 9
                case _ => stackTopInt = 1
            }


            if(expected_sym == "epsilon"){} 
            //If result from parse_tab is not 0
            else if (parse_tab(stackTopInt)(currtokInt) != 0){
                if( expected_sym == "epsilon") println()
                else {
                    //Get new elements for stack 
                    var prediction = parse_tab(stackTopInt)(currtokInt)
                    var topOfStack: List[String] = prod_tab(prediction - 1)
                    for ( x <- ((topOfStack.length - 1) to 0 by -1)) {
                        parse_stack += topOfStack(x)
                    }
                }
            }
            //If parse_tab = 0
            else {
                parse_error()
            }
        }

    }
  
}