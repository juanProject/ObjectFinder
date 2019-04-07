import java.io.FileWriter
import java.util.Date
import ClassIndex._
import scala.io._

object ObjectFinder{
  def main(args: Array[String]): Unit = {
    //val path = "C:/Users/chara/Downloads/CSVExemple.csv"
    //val jsonPath = "C:/Users/chara/Downloads/JSONExemple.json"
    //val csvPath = "C:/Users/chara/Downloads/csvFromJson.csv"
    val path = "D:/juanj/Downloads/CSVExemple.csv"
    val jsonPath = "D:/juanj/Downloads/JSONExemple.json"
    val csvPath = "D:/juanj/Downloads/csvFromJson.csv"

    val table = filterLinesFromFile(readLinesFromFile(path))

    val listOfObject = table.map( createObjectFromArray )
    //listOfObject.foreach( println )

    val jsonArray = listOfObject.map( objectToJsonString )
    //jsonArray.foreach( println )

    val jsonFile = writeJsonInFile( createFile(jsonPath), jsonArrayToJsonString(jsonArray) )
    jsonFile.close()

    val parsedJson = parseJson(readLinesFromFile(jsonPath).mkString(""))
    //parsedJson.foreach( line => println(line.mkString(",")) )

    val listOfObjectFromJson = parsedJson.map( createObjectFromParsedJson )
    //listOfObjectFromJson.foreach( println )

    val csvFile = writeCSVFile( createFile(csvPath), listOfObjectFromJson.map( objectToCSV ) )
    csvFile.close()
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

  /*
   * TODO : create function that returns a string from seq separated by ; ( for actor and film )
   * return json String for matched class
   */
  def objectToJsonString[A] ( c: A): String = c match {
    case Actor( name, filmsPlayed ) => "{\"name\":\"" + name + "\",\"filmsPlayed\":\"" + filmsPlayed.mkString(";") + "\"}"
    case Car(brand, countryOfBirth, maxSpeed, horsePower, speeds) => s"""{"brand":"$brand","countryOfBirth":"$countryOfBirth","maxSpeed":$maxSpeed,"horsePower":$horsePower,"speeds":$speeds}"""
    case Cat(name, race, age) => s"""{"name":"$name","race":"$race","age":$age}"""
    case Film(mainActors, dateOfRelease) => "{\"mainActors\":\"" + mainActors.mkString(";") + "\",\"dateOfRelease\":\"" + toFormatedDate(dateOfRelease) + "\"}"
    case Person(firstName, lastName, salary, numberOfChildren) => s"""{"firstName":"$firstName","lastName":"$lastName","salary":$salary,"numberOfChildren":$numberOfChildren}"""
    case _ => ""
  }

  def objectToCSV[A] ( c: A): String = c match {
    case Actor( name, filmsPlayed ) => name + "," + filmsPlayed.mkString(";")
    case Car(brand, countryOfBirth, maxSpeed, horsePower, speeds) => s"$brand,$countryOfBirth,$maxSpeed,$horsePower,$speeds"
    case Cat(name, race, age) => s"$name,$race,$age"
    case Film(mainActors, dateOfRelease) => mainActors.mkString(";") + "," + toFormatedDate(dateOfRelease)
    case Person(firstName, lastName, salary, numberOfChildren) => s"$firstName,$lastName,$salary,$numberOfChildren"
    case _ => ""
  }

  def toFormatedDate ( date: Date): String ={
    "%02d".format(date.getMonth + 1) + "/" + "%02d".format(date.getDate) + "/" + (date.getYear + 1900)
  }


  def createFile( fileName: String): FileWriter={
    new FileWriter(fileName)
  }

  def writeJsonInFile( file: FileWriter, jsonString: String): FileWriter ={
    file.write(jsonString)
    file
  }

  def jsonArrayToJsonString ( jsonArray: Array[String]): String ={
    "[" + jsonArray.mkString(",") + "]"
  }

  def parseJson ( json: String ) : Array[Array[String]] = {
    filterLinesFromFile(json.split("""},[{]|}]|\[[{]"""))
  }

  def getJsonValue ( jsonElem: String ): String = {
    val value = jsonElem.split("\"[ ]*:[ ]*\"?").last.trim
    if ( value.contains("\"")){
      value.replace("\"", "").trim
    } else {
      value
    }
  }

  def createObjectFromParsedJson ( parsedJson: Array[String] ): Any = parsedJson match{
    case empty if empty.length < 2 => "This class is not implemented yet"
    case actor if actor(1).contains("\"filmsPlayed\"") => Actor( getJsonValue(actor.head), semiColumnToSeq(getJsonValue(actor(1))) )
    case car if car.length == 5 => Car( getJsonValue(car.head), getJsonValue(car(1)), getJsonValue(car(2)).toInt, getJsonValue(car(3)).toInt, getJsonValue(car(4)).toInt )
    case cat if cat.length == 3 => Cat( getJsonValue(cat.head), getJsonValue(cat(1)), getJsonValue(cat(2)).toInt )
    case film if film.head.contains("\"mainActors\"") => Film( semiColumnToSeq(getJsonValue(film.head)), new Date(getJsonValue(film(1))) )
    case person if person.length == 4 => Person( getJsonValue(person.head), getJsonValue(person(1)), getJsonValue(person(2)).toInt, getJsonValue(person(3)).toInt )
    case _ => "This class is not implemented yet"
  }

  def writeCSVFile( file: FileWriter, csvArray: Array[String]): FileWriter ={
    csvArray.foreach( line => file.write(line + "\n") )
    file
  }

}