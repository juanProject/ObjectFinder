object ObjectFinder{
  def main(args: Array[String]): Unit = {
    val path = "C:/Users/chara/Downloads/CSVExemple.csv"
    filterLinesFromFile(readLinesFromFile(path)).foreach(println)
    //lines.foreach(println)
  }

  def readLinesFromFile(path: String): Array[String] = {
    import scala.io._
    Source.fromFile(path).getLines.toArray
  }

  def filterLinesFromFile(array: Array[String]): Array[String] = {
    var newArray = new Array[String](0)
    for(line <- array){
      newArray = line.split(",").map(_.trim)
    }
    newArray
  }
}