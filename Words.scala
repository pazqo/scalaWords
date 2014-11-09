// object Constants {
//     import scala.io.Source

//     val alpha = "abcdefghijklmnopqrstuvwxyz".toCharArray.toList

//     val vocals = "aeiou".toCharArray.toList

//     val consonants = "bcdfghjklmnpqrstvwxyz".toCharArray.toList

//     val voc_ita = Source.fromFile("data/dictionary_ita.txt").getLines().toSet
// }

object Words {

    type Anagramma = Set[String]

    type Occurr = Map[Char, Int]

    import scala.io.Source
    // import Constants._

    val alpha = "abcdefghijklmnopqrstuvwxyz".toCharArray.toList

    val vocals = "aeiou".toCharArray.toList

    val consonants = "bcdfghjklmnpqrstvwxyz".toCharArray.toList

    val voc_ita = Source.fromFile("data/dictionary_ita.txt").getLines().map(_.toLowerCase).toSet
    val voc_zingarelli = Source.fromFile("data/zingarelli.txt").getLines().map(_.toLowerCase).toSet

    val voc = voc_ita ++ voc_zingarelli

    val starting = (1 to 25).map( n => n -> voc.map(_.take(n)).toSet.filter(_.size==n)).toMap

    // type Occurrencies = Map[Char, Int]

    val allAnagrammi = voc.groupBy(_.sorted)

    implicit def stringToWord(s: String) = Word(s.replace(" ","").toLowerCase)
    implicit def mapToOccurrencies(s: Map[Char, Int]) = Occurrencies(s)

    implicit def wordsToVocabulary(voc: Set[String]) = Vocabulary(voc)



    case class Word(s: String) {
        val freqs = s.groupBy(identity).map({case (char, chars) => char -> chars.size}): Occurrencies

        lazy val (vocali, consonanti) = s.partition(vocals contains _)

        lazy val isWord = voc contains s
        
        lazy val isPalindromo = s == s.reverse
        lazy val isAntipodoPalindromo = s == s.antipodo
        lazy val isAntipodoPalindromoInverso = s == s.antipodoInverso

        lazy val antipodo = (s.head + s.tail.reverse)
        lazy val antipodoInverso = (s.init.reverse + s.last)
        lazy val unzipped = {
            val (w1, w2) = s.zipWithIndex.partition(_._2 % 2 == 0)
            (w1.map(_._1).mkString, w2.map(_._1).mkString)
        }

        lazy val anagrammi = allAnagrammi.getOrElse(s.sorted, List())

        lazy val subWords = freqs.subCombinations.filter(_.isValid).map(_.string).flatMap(_.anagrammi)

        lazy val sciarade = 
            (0 to s.size).map(i => (s.substring(0,i), s.substring(i,s.size))).filter({case (x,y) => x.isWord && y.isWord}).toList

        def multi(parts: Int): Stream[List[String]] = parts match {
            case 1 => if (s.isWord) Stream(List(s)) else Stream()
            case n => {
                val occurrence = s.freqs
                for {
                    a <- occurrence.subCombinations;
                    if a.isValid;
                    rest <- (occurrence minus a).string.multi(n-1)
                } yield a.string :: rest
            }
        }

        lazy val splits = 
            (0 to s.size).map(i => (s.substring(0,i), s.substring(i,s.size)))

        lazy val zeppe = (for {
            split <- splits
            char <- alpha
            if (split._1 + char + split._2).isWord
        } yield split._1 + char + split._2).distinct

        lazy val aggiuntaIniziale = 
            alpha.map(_ + s).filter(_.isWord)

        lazy val aggiuntaFinale =
            alpha.map(s + _).filter(_.isWord)

        lazy val cambi = (
            for {
                split <- splits.init
                oldChar = split._2.head
                newChar <- alpha.toSet - oldChar
                newWord = split._1 + newChar + split._2.tail
                if newWord.isWord
            } yield newWord).distinct.toList

        // def metagramma(t: Word): Stream[List[Word]] = 

    //     def EditDistance(t: String, m: Map.empty[String, Int]): Int = {
    //     if (s == "" || t == "") s.size + t.size
    //     else {
    //         val current = if (s.head == t.head) 0 else 1
    //         m + (s.tail + t -> s.tail.EditDistance(t)) + (s + t.tail -> s.EditDistance(t.tail)) + (s.tail + t.tail -> s.tail.EditDistance(t.tail))
    //         .min + current
    //     }
    // }
    }

    case class Occurrencies(occ: Map[Char, Int]) {

        val empty = Map.empty[Char, Int]

        val string = occ.map({case (char, freq) => char.toString * freq}).mkString.sorted

        val clear = occ.filter(_._2 != 0)

        def minus(m1: Occurrencies): Occurrencies = {
            occ.map({ case (char, freq) => 
                char -> (freq - m1.occ.getOrElse(char, 0))
            }).clear
        }

        def plus(m1: Occurrencies) = {
            val keys = occ.keys ++ m1.occ.keys
            keys.map( char => 
                char -> (occ.getOrElse(char, 0) + m1.occ.getOrElse(char, 0))
            )
        }

        val isValid = allAnagrammi contains string

        lazy val subCombinations: Stream[Map[Char,Int]] = {
            if (occ.isEmpty) Stream(Map.empty[Char,Int])
            else {
                val toAppend = for (i <- 0 to occ.head._2) yield (occ.head._1 -> i)
                for (rest <- occ.tail.subCombinations; h <- toAppend) yield rest + h
            }.map(_.clear).filter(_.size != 0)
        }

    }

    case class Vocabulary(voc: Set[String]) {
        lazy val anagrammi = voc.groupBy(_.sorted)
    }

    case class Board(s: String) {        
        lazy val board = s.split(" ").map(_.toArray)
        
        val show = board.map(_.mkString(" ")).mkString("\n")

        val side = board.size

        val allSquares = (for {
            i <- 0 until side
            j <- 0 until side 
        } yield (i,j)).toSet
    }

    case class Exploration(b: Board, current: List[(Int, Int)]) {

        val nowPos = current.headOption.getOrElse((0,0))

        val available = b.allSquares -- current.toSet

        val reachable = ({
            val (x, y) = nowPos
            for {
                i <- -1 to 1
                j <- -1 to 1
                if i != 0 || j != 0
                if x + i < b.side && x + i >= 0 && y + j < b.side && y + j >= 0
            } yield (x + i, y + j)}).toSet

        val nextPos = (reachable -- current).toStream

        val nextExplorations = nextPos.map( pos => Exploration(b, pos +: current) )
        
        lazy val visitedString = current.reverse.map(pos => b.board(pos._1)(pos._2)).mkString

        lazy val isValid = visitedString.isWord

        lazy val word = Stream(visitedString).filter(_.isWord)
 
        lazy val isStarting = starting(current.size) contains visitedString
    }

    val b = Board("ant tsi are")

    val exp = Exploration(b, List((0,0)))

    def exploreAcc(b: Board, current: List[(Int, Int)]): Stream[String] = {
        val exp = Exploration(b, current)
        if (exp.available.size == 0) {
            exp.word
        }
        else {
            (exp.nextExplorations
                .filter(x => x.current.size < 26 && x.isStarting)
                .flatMap(x => exploreAcc(b, x.current))
            ) ++ exp.word
        }
    }

    def explore(b: Board): Stream[String] = {
        b.allSquares.toStream.flatMap(x => exploreAcc(b, List(x))).distinct
    }

    def solve(b: Board): List[String] = {
        explore(b).sortBy(-_.size).take(15).toList
    }

    def metagramma(w1: String, w2: String): List[String] = {
        metagrammaAcc(w2, Stream(w1), Set(w1), List())
    }

    def metagrammaAcc(target: String, toVisit: Stream[String], visited: Set[String], xs: List[String]): List[String] = toVisit match {
        case x +: y => {
            if (x == target)
                xs :+ target
            else {
                val next = x.cambi.filterNot(visited contains _)
                metagrammaAcc(target, y ++ next, visited ++ next.toSet, xs :+ x)
            }
        }
        case _ => {
            List()
        }
    }

    // metagramma
    // parti da una parola, ottieni la parola target
    // exploration depth first
    // it's a graph, a-b are directly connected if there is a 1-letter change
    // word -> word.cambi (list of first level words), transform into a stream
    // for each word in first level, append it in the stream of cambi, this is part of the second level words
    // you do not want to navigate to a word which is already scheduled
    // when you reach the target word, you are sure that the route is the minumum.
    // TODO
    // I need to recover the word chain to that point
}