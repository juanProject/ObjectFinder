def main(args: Array[String]): Unit = {
  val lines = linesFromFile("/Users/vincent.vanhassel/ESGI-cours/4-IBC/scala/IdeaProjects/scala-spark-boilerplate/src/main/scala/com/fakir/samples/fileExample.csv")
  val withoutHeader = getRidOfHeader(lines)
  filterLinesFile(withoutHeader).foreach(println)
}

def linesFromFile(path: String): Array[String] = {
  import scala.io._
  Source.fromFile(path).getLines.toArray
}

def getRidOfHeader (array: Array[String]) : Array[String] = {
  val header = array.head
  array.filter(line => line != header)
}

def filterLinesFile(array: Array[String]) : Array[String] = {
  val dataFiltered = array.filter(line => line.split(", ")(2).toInt > 2000)
  val result = dataFiltered.filter(line => line.split(", ")(3).toInt > 1)
  result
}