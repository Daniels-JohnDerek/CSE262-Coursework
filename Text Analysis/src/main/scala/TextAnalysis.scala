/** 
    Simple Text Analysis Project
    CSE 262 - Programming Languages
    Fall 2017
    Name: JohnDerek Daniels
    Email: JRD319
*/

/**

brew install swi-prolog

Notes from Class: 
    -Few println at end of main 
    -println(list.mkString("\n")) -> Prints whole list
    -pick 5 words to do probability

    .split("[ ,.]").toSet

    wordcountmap.toSeq.sortBy(_._2).

*/

import scala.io.Source

object TextAnalysis {

    def main(args: Array[String]): Unit = {
        val songTuples = readList()
        val songTitle = songTuples.map(x => x._2)
        val artistNames = songTuples.map(x => x._1)

        val wordsMostCommon = findNonTrivial(songTitle)
        val artistMostTop40 = findArtistMost40(artistNames)
        val artistMostNo1 = findArtistMost1(songTuples)

        println("\nMost common nontrivial word: "+wordsMostCommon+
            "\nArtists with most Top40 hits: "+artistMostTop40+
            "\nArtists with most Top1 hits: "+artistMostNo1)

        findProbability(wordsMostCommon, songTuples, songTitle)
    }

    def findProbability(top10Word: Seq[(String,Int)], allSongs: List[(String,List[String],Int)],
        songTitles: List[List[String]]): Unit = {


        val songs: List[(List[String],Int)] = allSongs.map( x => (x._2, x._3))
        val numTop1Song = (songs.filter(_._2 == 1)).length
        val no1Songs = songs map {
            case (songTitle, 1) => (songTitle)
            case (songTitle, 0) => ("")
            case (_,_) => ("")
        }
        
        val no1SongsFormat = no1Songs.filter(_ != (""))
        val songsFiltered: List[List[String]] = songTitles.filter(no1SongsFormat.contains(_))

        val allWordsinNo1Titles = songsFiltered.flatten
        val allWordsinTitles = songTitles.flatten

        val top5Word = top10Word.drop(5)
        top5Word.foreach (x => {    
            val wordInNo1Titles = allWordsinNo1Titles.groupBy(identity).mapValues(_.size)(x._1)
            val wordInAllTitles = allWordsinTitles.groupBy(identity).mapValues(_.size)(x._1)
            val aBA: Float = wordInNo1Titles.toFloat/wordInAllTitles
            val aBB: Double = wordInNo1Titles.toFloat/numTop1Song

            println("\nCount of No1 songs containing the word "+x._1+" = "+wordInNo1Titles+" (A&B)\nCount of songs with the word "+x._1+" = "+wordInAllTitles+" (A)\nCount of #1 songs: "+numTop1Song+" (B)\n" +
                wordInNo1Titles+"/"+wordInAllTitles+" = "+aBA+" ((A&B)/A)\n" +
                wordInNo1Titles+"/"+numTop1Song+" = "+aBB+" ((A&B)/B)\n")
        })

    

    }

    def findArtistMost1(allSongs: List[(String,List[String],Int)]): Seq[(String,Int)] = {
        val artistAndNum = allSongs.map( x => (x._1, x._3))
        val num1Artist = artistAndNum map {
            case (artist, 1) => (artist, 1) 
            case (artist, 0) => ("","")
            case (_,_) => ("","")
        }
        val num1Artistfilter = num1Artist.filter(_ != ("",""))
        val num1ArtistTot = num1Artistfilter.groupBy(identity).mapValues(_.size)
        val num1ArtistTotOrder = num1ArtistTot.toSeq.sortBy(_._2)
        val top10ArtistNo1 = num1ArtistTotOrder.takeRight(10)
        val artistMostNo1 = top10ArtistNo1.map( x => (x._1._1, x._2))
        
        return artistMostNo1
    }

    def findArtistMost40(artistList: List[String]): Seq[(String,Int)] = {
        val artistOccur = artistList.groupBy(identity).mapValues(_.size)
        val artistOccurOrder = artistOccur.toSeq.sortBy(_._2)
        val artistTop10 = artistOccurOrder.takeRight(10)
        return artistTop10
    }

    def findNonTrivial(titleList: List[List[String]]): Seq[(String,Int)] = {
        val trivialWords: List[String] = List("TO","OF","IN","FOR","ON","WITH","AT","BY","A","THE",
                                            "AND","THAT","I","IT","NOT","HE","AS","YOU","THIS","BUT",
                                            "HIS","THEY","HER","SHE","OR","AN","WILL","MY","ALL",
                                            "THERE","IS","BE","DO","IM")

        val titleFlat = titleList.flatten
        val titleFiltered = titleFlat.filter(!trivialWords.contains(_))
        val wordOccur = titleFiltered.groupBy(identity).mapValues(_.size)
        val wordOccurOrder = wordOccur.toSeq.sortBy(_._2)
        val wordTop10 = wordOccurOrder.takeRight(10)
        return wordTop10
    }

    def readList(): List[(String,List[String],Int)] = {
        val filename = "top40.sql"
        val fileContent = Source.fromFile(filename).getLines.toList
        val contentMap = fileContent.map( x => {
            val strings = x.split("'")
            (clean(strings(1)), clean(strings(3)).split("\\s+").toList, clean(strings(4)).replaceAll(" ","").toInt)
        })
        return contentMap
    }

    def clean(input: String): String = {
        val input0 = input.replaceAll("\\*", "")
        val input1 = input0.replaceAll("\\(", "")
        val input2 = input1.replaceAll("\\)", "")
        val input3 = input2.replaceAll(";", "")
        val input4 = input3.replaceAll(",", "")
        val input5 = input4.replaceAll("/", "")
        return input5
    }

}