import scala.io.Source
import scala.util.matching.Regex

case class Address(street: String, house_number: Option[String] = None, house_number_addition: Option[String] = None)

object Address extends App {
  def addressParting(address: Address) : Address = ???

  val regexes: List[Regex] =
    Source
      .fromFile("src/main/scala/splitsstraathuisnr.ini")
      .getLines
      .map(line => ("(?i)" + line).r)
      .toList

  println(regexes)

  val addresses: List[String] =
    Source
      .fromFile("src/main/scala/test.txt")
      .getLines
      .toList

  val (matched, unmatched) =
    addresses.partition(address => regexes.exists(_.findFirstIn(address) != None))

  println(s"MATCHED:\n$matched\n")
  println(s"UNMATCHED:\n$unmatched\n")
}