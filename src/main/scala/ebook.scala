package ebook
import scala.collection.immutable.VectorMap

object Main extends App {
  def wc(document: String): VectorMap[String, Int] = 
    val docWithoutNewLines = replaceNewLinesWithSpaces(document)
    val docWithoutFormatting = stripFormattingCharacters(docWithoutNewLines)
    val docuWithoutDoubleSpaces = stripDoubleSpaces(docWithoutFormatting)
    val listOfWords = convertToSequence(docuWithoutDoubleSpaces)
    val lowerWords = convertToLowerCase(listOfWords)
    val mapOfWords = convertToMap(lowerWords)

  def replaceNewLinesWithSpaces(text: String): String =
    text.replaceAll("\n", " ")

  def stripFormattingCharacters(text: String): String = 
    text.replaceAll(";", "")
    .replaceAll(",", "")
    .replaceAll("\\.", "")
    .replaceAll("—", "")
    .replaceAll("!", "")
    .replaceAll("\\?", "")

  def stripDoubleSpaces(text: String): String = 
    text.replaceAll(" +", " ")

  def convertToSequence(text: String): Seq[String] =
    text.split(" ").filter(_.trim != "").toSeq

  def convertToLowerCase(text: Seq[String]): Seq[String] = 
    text.map(_.toLowerCase)

  def convertToMap(text: Seq[String]): Map[String, Int] = ???
}