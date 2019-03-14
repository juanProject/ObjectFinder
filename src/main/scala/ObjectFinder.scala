import scala.collection.mutable.ArrayBuffer

object ObjectFinder{
  def main(args: Array[String]): Unit = {
    val path = "C:/Users/chara/Downloads/CSVExemple.csv"
    //val path = "D:/juanj/Downloads/CSVExemple.csv"
    val table = filterLinesFromFile(readLinesFromFile(path))
    /*println(
      for{
        n <- table
        na <- n
      } yield (na).toString
    )*/
    //lines.foreach(println)
    table.foreach( line => line.foreach(println) )
  }

  def readLinesFromFile(path: String): Array[String] = {
    import scala.io._
    Source.fromFile(path).getLines.toArray
  }

  def filterLinesFromFile(array: Array[String]): Array[Array[String]] = {
    val result: Array[Array[String]] = array.map(line => line.split(",").map(_.trim) )
    result
  }
}