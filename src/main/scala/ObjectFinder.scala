import java.io.FileWriter
import java.util.Date
import ClassIndex._
import scala.io._

object ObjectFinder{
  def main(args: Array[String]): Unit = {
    /*
      Defining Directory path
    */
    val path = "C:/Temp/CSVExemple.csv"
    val jsonPath = "C:/Temp/JSONExemple.json"
    val csvPath = "C:/Temp/csvFromJson.csv"
    /*
      Writing JSON file from CSV
    */
    val table = filterLinesFromFile(readLinesFromFile(path))
    val listOfObject = table.map( createObjectFromArray )
    val jsonArray = listOfObject.map( objectToJsonString )
    val jsonFile = writeJsonInFile( createFile(jsonPath), jsonArrayToJsonString(jsonArray) )
    jsonFile.close()
    /*
      Rewriting CSV file from generated JSON
    */
    val parsedJson = parseJson(readLinesFromFile(jsonPath).mkString(""))
    val listOfObjectFromJson = parsedJson.map( createObjectFromParsedJson )
    val csvFile = writeCSVFile( createFile(csvPath), listOfObjectFromJson.map( objectToCSV ) )
    csvFile.close()
  }

  /*
    readLinesFromFile is a function to return file content line by line in a String array
  */
  def readLinesFromFile(path: String): Array[String] = {
    Source.fromFile(path).getLines.toArray
  }

  /*
    filterLinesFromFile splits a String array using Char "," and trims whitespaces
  */
  def filterLinesFromFile(array: Array[String]): Array[Array[String]] = {
    array.map(line => line.split(",").map(_.trim) )
  }

  /*
    semiColumnToSeq splits a String array using Char ";" , and trims whitespaces
  */
  def semiColumnToSeq(string: String) : Seq[String] = {
    string.split(";").map(_.trim).toSeq
  }

  /*
    createObjectFromArray creates an Object according to the contents of the Array
  */
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
    objectToJsonString creates a String to generate JSON based on Type Matching of the Array in parameter
  */
  def objectToJsonString[A] ( c: A): String = c match {
    case Actor( name, filmsPlayed ) => "{\"name\":\"" + name + "\",\"filmsPlayed\":\"" + filmsPlayed.mkString(";") + "\"}"
    case Car(brand, countryOfBirth, maxSpeed, horsePower, speeds) => s"""{"brand":"$brand","countryOfBirth":"$countryOfBirth","maxSpeed":$maxSpeed,"horsePower":$horsePower,"speeds":$speeds}"""
    case Cat(name, race, age) => s"""{"name":"$name","race":"$race","age":$age}"""
    case Film(mainActors, dateOfRelease) => "{\"mainActors\":\"" + mainActors.mkString(";") + "\",\"dateOfRelease\":\"" + toFormatedDate(dateOfRelease) + "\"}"
    case Person(firstName, lastName, salary, numberOfChildren) => s"""{"firstName":"$firstName","lastName":"$lastName","salary":$salary,"numberOfChildren":$numberOfChildren}"""
    case _ => ""
  }

  /*
    objectToCSV creates a String to generate CSV based on Type Matching
  */
  def objectToCSV[A] ( c: A): String = c match {
    case Actor( name, filmsPlayed ) => name + "," + filmsPlayed.mkString(";")
    case Car(brand, countryOfBirth, maxSpeed, horsePower, speeds) => s"$brand,$countryOfBirth,$maxSpeed,$horsePower,$speeds"
    case Cat(name, race, age) => s"$name,$race,$age"
    case Film(mainActors, dateOfRelease) => mainActors.mkString(";") + "," + toFormatedDate(dateOfRelease)
    case Person(firstName, lastName, salary, numberOfChildren) => s"$firstName,$lastName,$salary,$numberOfChildren"
    case _ => ""
  }

  /*
    toFormatedDate formats the date to a comprehensible format
  */
  def toFormatedDate ( date: Date): String ={
    "%02d".format(date.getMonth + 1) + "/" + "%02d".format(date.getDate) + "/" + (date.getYear + 1900)
  }

  /*
    createFile creates a FileWriter with the path + name
  */
  def createFile( fileName: String): FileWriter={
    new FileWriter(fileName)
  }

  /*
    writeJsonInFile writes the JSON using the FileWriter and a String
  */
  def writeJsonInFile( file: FileWriter, jsonString: String): FileWriter ={
    file.write(jsonString)
    file
  }

  /*
    jsonArrayToJsonString transforms an Array of String to a signle String
  */
  def jsonArrayToJsonString ( jsonArray: Array[String]): String ={
    "[" + jsonArray.mkString(",") + "]"
  }

  /*
    parseJson splits a String using the Regex characters
  */
  def parseJson ( json: String ) : Array[Array[String]] = {
    filterLinesFromFile(json.split("""},[{]|}]|\[[{]"""))
  }

  /*
    getJsonValue parses and removes special chars to get Value from jsonString
  */
  def getJsonValue ( jsonElem: String ): String = {
    val value = jsonElem.split("\"[ ]*:[ ]*\"?").last.trim
    if ( value.contains("\"")){
      value.replace("\"", "").trim
    } else {
      value
    }
  }

  /*
    createObjectFromParsedJson uses an Array to create an Object based on Type Matching
  */
  def createObjectFromParsedJson ( parsedJson: Array[String] ): Any = parsedJson match{
    case empty if empty.length < 2 => "This class is not implemented yet"
    case actor if actor(1).contains("\"filmsPlayed\"") => Actor( getJsonValue(actor.head), semiColumnToSeq(getJsonValue(actor(1))) )
    case car if car.length == 5 => Car( getJsonValue(car.head), getJsonValue(car(1)), getJsonValue(car(2)).toInt, getJsonValue(car(3)).toInt, getJsonValue(car(4)).toInt )
    case cat if cat.length == 3 => Cat( getJsonValue(cat.head), getJsonValue(cat(1)), getJsonValue(cat(2)).toInt )
    case film if film.head.contains("\"mainActors\"") => Film( semiColumnToSeq(getJsonValue(film.head)), new Date(getJsonValue(film(1))) )
    case person if person.length == 4 => Person( getJsonValue(person.head), getJsonValue(person(1)), getJsonValue(person(2)).toInt, getJsonValue(person(3)).toInt )
    case _ => "This class is not implemented yet"
  }

  /*
    writeCSVFile uses the CSVArray to generate the new CSV File
  */
  def writeCSVFile( file: FileWriter, csvArray: Array[String]): FileWriter ={
    csvArray.foreach( line => if(line!="") file.write(line + "\n") )
    file
  }
}