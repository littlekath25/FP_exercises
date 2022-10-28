package ebook
import scala.collection.immutable.VectorMap
import scala.io.Source
import scala.util.{Try, Success, Failure}
import scala.util.control.Exception.allCatch

def wc(document: String): VectorMap[String, Int] = 
  val result = 
    sortMapByHighestValue(
      convertToMap(
        convertToLowerCase(
          convertToSequence(
            stripDoubleSpaces(
              stripFormattingCharacters(
                replaceNewLinesWithSpaces(document)
                )
              )
            )
          )
        )
      )
  result

def replaceNewLinesWithSpaces(text: String): String =
  text.replaceAll("\n", " ")

def stripFormattingCharacters(text: String): String = 
  text.replaceAll(";", "")
  .replaceAll(",", "")
  .replaceAll("\\.", "")
  .replaceAll("—", "")
  .replaceAll("!", "")
  .replaceAll("\\?", "")
  .replaceAll(":", "")

def stripDoubleSpaces(text: String): String =
  text.replaceAll(" +", " ")

def convertToSequence(text: String): Seq[String] =
  text.split(" ").filter(_.trim != "").toSeq

def convertToLowerCase(text: Seq[String]): Seq[String] = 
  text.map(_.toLowerCase)

def convertToMap(text: Seq[String]): Map[String, Int] =
  val wcMap = scala.collection.mutable.Map[String, Int]()
  for word <- text do
    if (wcMap.contains(word)) then
      val count = wcMap(word)
      wcMap(word) = count + 1
    else
      wcMap(word) = 1
    end if
  end for
  wcMap.toMap

def sortMapByHighestValue(text: Map[String, Int]): VectorMap[String, Int] =
  VectorMap(text.toSeq.sortWith((t1, t2) => t1._2 > t2._2): _*)

object Main extends App {
  def readFromFile(name: String): Try[String] =
    allCatch.withTry(Source.fromResource(name).mkString)

  val maybeFile = readFromFile("text.txt")

  maybeFile match {
    case Success(value) => println(wc(value))
    case Failure(exception) => println("Couldn't read the file: $exception")
  }
}