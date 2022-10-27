package ebook
import scala.collection.immutable.VectorMap
import scala.io.Source

def wc(document: String): VectorMap[String, Int] = 
  val docWithoutNewLines = replaceNewLinesWithSpaces(document)
  val docWithoutFormatting = stripFormattingCharacters(docWithoutNewLines)
  val docuWithoutDoubleSpaces = stripDoubleSpaces(docWithoutFormatting)
  val listOfWords = convertToSequence(docuWithoutDoubleSpaces)
  val lowerWords = convertToLowerCase(listOfWords)
  val mapOfWords = convertToMap(lowerWords)
  val finalResult = sortMapByHighestValue(mapOfWords)
  finalResult

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
  val document = Source.fromResource("text.txt").mkString
  println(wc(document))
}