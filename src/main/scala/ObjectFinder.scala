import java.util.Date
import ClassIndex._
import scala.io._

object ObjectFinder{
  def main(args: Array[String]): Unit = {
    val path = "C:/Users/chara/Downloads/CSVExemple.csv"
    //val path = "D:/juanj/Downloads/CSVExemple.csv"
    val table = filterLinesFromFile(readLinesFromFile(path))
    val listOfObject = table.map(line => createObjectFromArray(line) )
    listOfObject.foreach( line => println(line) )
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
    array.map(line => line.split(",").map(_.trim) )
  }

  /*
  semiColumnToSeq split array by ";" , trim it and return Seq[string]
   */
  def semiColumnToSeq(string: String) : Seq[String] = {
    string.split(";").map(_.trim).toSeq
  }
  /*
  def stringIsInt(string: String): Either[Exception, Int] = {
    try Right(string.toInt)
    catch { case e: Exception => Left(e) }
  }
  */

  /*
  * try to fit Array to an object in classIndex
  * */
  def createObjectFromArray(array: Array[String] ): Any = array match{
    //case noResult if noResult.length < 2 => "this class is not implemented yet"
    case actor if actor(1).contains(";") => Actor( actor.head, semiColumnToSeq( actor(1) ) )
    case car if car.length == 5 => Car( car.head, car(1), car(2).toInt, car(3).toInt, car(4).toInt )
    case cat if cat.length == 3 => Cat( cat.head, cat(1), cat(2).toInt )
    case film if film(0).contains(";") => Film( semiColumnToSeq(film.head), new Date ( film(1) ) )
    case person if person.length == 4 => Person( person.head, person(1), person(2).toInt, person(3).toInt )
    case _ => "This class is not implemented yet"
  }
}