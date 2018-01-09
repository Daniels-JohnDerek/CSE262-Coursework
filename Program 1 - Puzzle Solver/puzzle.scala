/**
* JohnDerek Daniels
* Jrd319
* CSE 262
* Program #1
**/


object PuzzleSolver {

    var puzzArrayFinal = scala.collection.mutable.Set(0)
    val rand = scala.util.Random

    def main (args: Array[String]): Unit = {

        var puzzArray = Array(0,0,0,0,0,0,0)
        var x = 0
        var assignNum = true
        var checkNum = 0
        var config = false
        var dependents = false
        println("Running...")

            var aa = 0; var bb = 0; var cc = 0; var dd = 0; var ee = 0; var ff = 0; var gg = 0

            for(aa <- 1 to 19) {
                if(config == true) {}
                else {
                    for(bb <- 2 to 19) {
                        if(config == true) {}
                        else {
                            for(cc <- 3 to 19){
                                if(config == true) {}
                                else {
                                    for(dd <- 4 to 19){
                                        if(config == true) {}
                                        else {
                                            for(ee <- 5 to 19){
                                                if(config == true) {}
                                                else {
                                                    for(ff <- 6 to 19){
                                                        if(config == true) {}
                                                        else {
                                                            for(gg <- 7 to 19){
                                                                if(config == true) {}
                                                                else {
                                                                    var j = dd
                                                                    var k = aa
                                                                    var n = bb
                                                                    var o = ee
                                                                    var pp = ff
                                                                    var r = gg
                                                                    var s = cc
                                                                    puzzArrayFinal += (j,k,n,o,pp,r,s)

                                                                    var a = 76 - j - k - n - 2*o - pp - r - s
                                                                    if(numCheck(a) == false) {}
                                                                    else {
                                                                        var b = j + n + o
                                                                        if(numCheck(b) == false) {}
                                                                        else {
                                                                            var c = -38 + k + o + pp + r + s
                                                                            if(numCheck(c) == false) {}
                                                                            else {
                                                                                var d = j + k + o
                                                                                if(numCheck(d) == false) {}
                                                                                else {
                                                                                    var e = -38 + k + n + o + pp + r
                                                                                    if(numCheck(e) == false) {}
                                                                                    else {
                                                                                        var f = 38 - j - k - n - o - pp
                                                                                        if(numCheck(f) == false) {}
                                                                                        else {
                                                                                            var g = 38 - k - o - r
                                                                                            if(numCheck(g) == false) {}
                                                                                            else {
                                                                                                var h = -38 + n + o + pp + r + s
                                                                                                if(numCheck(h) == false) {}
                                                                                                else {
                                                                                                    var i = 38 - j - k - n - o - r
                                                                                                    if(numCheck(i) == false) {}
                                                                                                    else {
                                                                                                        var l = 38 - pp - s
                                                                                                        if(numCheck(l) == false) {}
                                                                                                        else {
                                                                                                            var m = 38 - n - o - pp
                                                                                                            if(numCheck(m) == false) {}
                                                                                                            else {
                                                                                                                var q = 38 - r - s
                                                                                                                if(numCheck(q) == false) {}
                                                                                                                else {
                                                                                                                    if (a + b + c == 38){
                                                                                                                        config = true
                                                                                                                        println("  "+a+" "+b+" "+c)
                                                                                                                        println(" "+d+" "+e+" "+f+" "+g)
                                                                                                                        println(""+h+" "+i+" "+j+" "+k+" "+l)
                                                                                                                        println(" "+m+" "+n+" "+o+" "+pp)
                                                                                                                        println("  "+q+" "+r+" "+s)
                                                                                                                    }   
                                                                                                                }
                                                                                                            }
                                                                                                        }
                                                                                                    }
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }

                                                            puzzArrayFinal = puzzArrayFinal.empty

                                                            }
                                                        }    
                                                    }
                                                }    
                                            }
                                        }    
                                    }
                                }    
                    
                            }
                        }    
                    }       
                }
            }

    }

    def numCheck (n: Int): Boolean = {
        if (n > 19 || n < 1) return false
        if (puzzArrayFinal.contains(n)) return false
        else {
            puzzArrayFinal += (n)
            return true
        }  
    }


}