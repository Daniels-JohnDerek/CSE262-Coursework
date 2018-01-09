/* 
    FlexNumber class for Pascal Compiler
    CSE 262 - Programming Languages
    Fall 2017
    J. Femister
*/

case class FlexNumber(str:String) {
    var ival: Int = 0;
    private var dval: Double = 0.0;
    private var ntype: Int = 0;
    private val INT = 1;
    private val FLOAT = 2;
    def this(i: Int) = { this(""); ival = i; ntype = INT; }
    def this(d: Double) = { this(""); dval = d; ntype = FLOAT; }
    def this() = { this(""); ival = 0; ntype = 0; }
    def this(b: Boolean) = { this(""); ntype = INT; if (b) ival = 1 else ival = 0 }
    def toBoolean = if (ival == 1) true else false

    override def toString = if (ntype == INT) s"FlexNumber(${ival.toString})" else s"FlexNumber(${dval.toString})"

    if (str contains ".") { 
        dval = str.toDouble
        ntype = FLOAT
    } else if (str.length > 0){ 
        ival = str.toInt 
        ntype = INT 
    } else {
        ival = 0;
        ntype = INT
    }

    def convert(n1: FlexNumber, n2:FlexNumber) = {
        var d0 = 0.0
        var d1 = 0.0
        if (n1.ntype == FLOAT)
            d0 = n1.dval
        else
            d0 = n1.ival.toDouble
        if (n2.ntype == FLOAT)
            d1 = n2.dval;
        else
            d1 = n2.ival.toDouble;
        (d0, d1)
    }

    def + (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            val res = convert(this, n);
            new FlexNumber(res._1 + res._2);
        } else {
            new FlexNumber(ival + n.ival);
        }
    }

    def - (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            val res = convert(this, n);
            new FlexNumber(res._1 - res._2);
        } else {
            new FlexNumber(ival - n.ival);
        }
    }

    def * (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            val res = convert(this, n);
            new FlexNumber(res._1 * res._2);
        } else {
            new FlexNumber(ival * n.ival);
        }
    }

    def / (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            val res = convert(this, n);
            new FlexNumber(res._1 / res._2);
        } else {
            new FlexNumber(ival / n.ival);
        }
    }

    def % (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            println("Operator % only valid in Ints")
            new FlexNumber(0)
        } else {
            new FlexNumber(ival % n.ival);
        }
    }

    def == (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            val res = convert(this, n);
            new FlexNumber(res._1 == res._2)
        } else {
            new FlexNumber(ival == n.ival)
        }
    }

    def != (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            val res = convert(this, n);
            new FlexNumber(res._1 != res._2)
        } else {
            new FlexNumber(ival != n.ival)
        }
    }

    def < (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            val res = convert(this, n);
            new FlexNumber(res._1 < res._2)
        } else {
            new FlexNumber(ival < n.ival)
        }
    }

    def <= (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            val res = convert(this, n);
            new FlexNumber(res._1 <= res._2)
        } else {
            new FlexNumber(ival <= n.ival)
        }
    }

    def > (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            val res = convert(this, n);
            new FlexNumber(res._1 > res._2)
        } else {
            new FlexNumber(ival > n.ival)
        }
    }

    def >= (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            val res = convert(this, n);
            new FlexNumber(res._1 >= res._2)
        } else {
            new FlexNumber(ival >= n.ival)
        }
    }

    def & (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            println("Operator & only valid in Ints")
            new FlexNumber(0)
        } else {
            new FlexNumber(ival & n.ival);
        }
    }

    def | (n: FlexNumber) = {
        if (ntype == FLOAT || n.ntype == FLOAT) {
            println("Operator | only valid in Ints")
            new FlexNumber(0)
        } else {
            new FlexNumber(ival | n.ival);
        }
    }

}

object FlexNumber {
    def apply(s: String) = new FlexNumber(s)
    def apply(i : Int) = new FlexNumber(i)
}

