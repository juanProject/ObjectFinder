import scala.collection.mutable.ArrayBuffer

object ObjectFinder{
  def main(args: Array[String]): Unit = {
    val path = "C:/Users/chara/Downloads/CSVExemple.csv"
    val table = filterLinesFromFile(readLinesFromFile(path))
    println(
      for{
        n <- table
        na <- n
      } yield (na).toString
    )
    //lines.foreach(println)
  }

  def readLinesFromFile(path: String): Array[String] = {
    import scala.io._
    Source.fromFile(path).getLines.toArray
  }

  def filterLinesFromFile(array: Array[String]): Array[Array[String]] = {
    val rows = ArrayBuffer[Array[String]]()
    for(line <- array){
      rows += line.split(",").map(_.trim)
    }
    rows.toArray[Array[String]]
    /*
    var newArray = new Array[String](0)
    for(line <- array){
      newArray = line.split(",").map(_.trim)
    }
    newArray
    */
  }
}