/** 
    CSE 262 - Assignment 2
    Description: Table driven scanner for the calculator language in PLP, 4th Ed
    Author: JohnDerek Daniels
    Email: Jrd319
    Date: September 2017
*/

// List of tokens to be recognized
case object lparen
case object rparen
case object plus
case object minus
case object times
case object div
case object assign
case object read    // keyword
case object write   // keyword
case class Id(i:String)
case class Number(n:String)

case object skip
case object error
case object eof

abstract class Action
case object Move extends Action
case object Recognize extends Action
case object Error extends Action

object TableDrivenScanner extends App {
   // Special chars
    val newline = '\n'
    val EOF = '$'

    // Character categories/sets
    val whitespace = Set(' ', '\t', '\n')
    val digits = ('0' to '9').toSet
    val letters = ('A' to 'Z').toSet ++ ('a' to 'z').toSet
    val punctuation = Map('(' -> lparen, ')' -> rparen, '+' -> plus, 
                        '-' -> minus, '*' -> times)


    val keyword_tab = Map( "read" -> read, "write" -> write)

    type State = Int 
    type Token = Int
    case class ScanTabCell(var action:Action, var new_state:State) 

    // Read in entire test file into a single string
    val source = io.Source.fromFile("testexpr.txt").getLines.toList.mkString("\n")
    println(source)
    println

    // Method to return a single char
    var i = -1
    def readchar = { i += 1; if (i < source.length) source(i) else EOF }
    def unreadchars(n:Int) = { i -= n }

    // Read in scan_tab and token_tab from a file
    val scan_tab = Array.ofDim[State](18,15)
    val token_tab = new Array[String](18)
    val table = io.Source.fromFile("table.txt").getLines.toArray
    var lenCount = 0
    var lenStr = ""
    var strInt: State = 0

    for( x <- 0 to 17){
        lenStr = table(x).mkString
        var lenStrFor = lenStr.split("   |  | ").map(_.trim)
        for(y <- 0 to 15){
            if(y != 15) {
                strInt = lenStrFor(y).toInt
                scan_tab(x)(y) = strInt
            }
            else {
                token_tab(x) = lenStrFor(y)
            }
        }
    }
    
    //For loop with token_tab case statments to turn from string to object
    for (x <- token_tab){
        x match{
            case "0" => 0
            case "div" => object div
            case "lparen" => object lparen
            case "rparen" => object rparen
            case "plus" => object plus
            case "minus" => object minus
            case "times" => object times
            case "assign" => object assign
            case "number" => object number
            case "id" => object id
            case "whitespace" => object whitespace
            case "comment" => object comment
        }
    }

            
    // Fill in code to read in the tables from table.txt

    var start_state: State = 1

    var tok = ""
    var remembered_chars = scala.collection.mutable.Set[Char]()

    // Main method (called by the parser) to get the next token
    def nexttoken = {
        println("Enter")
        var cur_state: State = start_state
        var image: String = null
        var remembered_state: State = 0
        var tokenComplete = false 

        while (tokenComplete == false) {

            var cur_char = readchar
            var cur_charNum = 0

            println(cur_char)

            if (letters contains cur_char) cur_charNum = 13
            else if (digits contains cur_char) cur_charNum = 12
            else {
                cur_char match{
                    case '\t' => cur_charNum = 1
                    case ' ' => cur_charNum = 1
                    case '\n' => cur_charNum = 2
                    case '/' => cur_charNum = 3
                    case '*' => cur_charNum = 4
                    case '(' => cur_charNum = 5
                    case ')' => cur_charNum = 6
                    case '+' => cur_charNum = 7
                    case '-' => cur_charNum = 8
                    case ':' => cur_charNum = 9
                    case '=' => cur_charNum = 10
                    case '.' => cur_charNum = 11
                    case _ => cur_charNum = 14
                }
            }

            println(cur_charNum)

            var actionCell = ScanTabCell(Error,0)
            var next_state = 0

            if(scan_tab(cur_state)(cur_charNum) == 0){
                if(token_tab(cur_state) == 0){
                    actionCell = ScanTabCell(Error,cur_state) }
                else {
                    actionCell = ScanTabCell(Recognize,cur_state) }
            }
            else {
                actionCell = ScanTabCell(Move,cur_state) 
                next_state = scan_tab(cur_state)(cur_charNum)
            }

            // The code for the algorithm in Fig 2.11 goes in here

            actionCell.action match{
                case Move => {
                    if (token_tab(cur_charNum) != 0){
                        remembered_state = cur_state
                        remembered_chars = remembered_chars.empty
                    }
                    remembered_chars += cur_char
                    cur_state = next_state
                }
                case Recognize => {
                    tok = token_tab(cur_charNum)
                    tokenComplete = true
                    unreadchars(1)
                }
                case Error =>{
                    if (remembered_state != 0) {
                        tok = token_tab(remembered_state)
                        unreadchars(remembered_chars.size)
                        image.slice(0,(image.length - remembered_chars.size))
                        tokenComplete = true
                    }
                    else {
                        println("Error message")
                    }
                }
                case _ => println("Error no action matched")
            }

            image += cur_char
        }

        var ret = 1
        ret

    }

    var t = nexttoken
    
    /** Test program
    var t = nexttoken
    while (t != eof) {
        //println(t)
        t = nexttoken
    }
    **/
}

