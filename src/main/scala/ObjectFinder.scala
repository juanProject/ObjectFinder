import java.util.Date

import ClassIndex._

import scala.collection.mutable.ArrayBuffer
import scala.io._

object ObjectFinder{
  def main(args: Array[String]): Unit = {
    //val path = "C:/Users/chara/Downloads/CSVExemple.csv"
    val path: String = "D:/juanj/Downloads/CSVExemple.csv"
    val table: Array[Array[String]] = filterLinesFromFile(readLinesFromFile(path))


    //val objectTable: Array[Array[Object]] = filterSemiColumnFromFile(table)
    /*println(
      for{
        n <- table
        na <- n
      } yield (na).toString
    )*/
    //lines.foreach(println)

    table.foreach( line => line.foreach(println) )
  }

  /*
  readLinesFromFile return file content line by line in an array
   */
  def readLinesFromFile(path: String): Array[String] = {
    Source.fromFile(path).getLines.toArray
  }

  /*
  filterLinesFromFile split array by character "," and trim
   */
  def filterLinesFromFile(array: Array[String]): Array[Array[String]] = {
    val result = array.map(line => line.split(",").map(_.trim) )
    result
  }

  /*
  semiColumnToSeq split array by ";" , trim it and return Seq[string]
   */
  def semiColumnToSeq(string: String) : Seq[String] = {
    val splitted = string.split(";").map(_.trim)
    splitted.toSeq
  }

  /*
  function not used but keeped in case of who knows
   */
  def filterSemiColumnFromFile(array: Array[Array[String]]): Array[Array[Object]] = {
    val result = array.map( line => line.map( elem => if ( elem.contains(";") ) semiColumnToSeq(elem) else elem ) )
    result
  }

  /*
  * try to fit Array to an object in classIndex
  * */
  def fromArrayToObject( array: Array[String] ): Any = array match{
    case headIsSeq if headIsSeq.head.contains(";") => Film(semiColumnToSeq(headIsSeq.head), new Date (headIsSeq(2)) )
    case secondIsSeq if secondIsSeq(2).contains(";") => Actor( secondIsSeq.head, semiColumnToSeq(secondIsSeq(2)) )
    case _ => Person("not implemented", "yet", 5000, 2)
  }
}