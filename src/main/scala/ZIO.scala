import scala.io.Source
import scala.util.matching.Regex

case class Address(street: String, house_number: Option[String] = None, house_number_addition: Option[String] = None)

object Address extends App {
  def addressParting(address: Address) : Address = ???

  val rules: List[Regex] =
    Source
      .fromFile("src/main/scala/splitsstraathuisnr.ini")
      .getLines
      .map(line => ("(?i)" + line).r)
      .toList

  val input: List[String] =
    Source
      .fromFile("src/main/scala/test.txt")
      .getLines
      .toList

  def filterAddressesWithDigits(regexes: List[Regex], addresses: List[String]): (List[String], List[String]) =
    addresses.partition(address => regexes.exists(_.findFirstIn(address) != None))

  val (matched, unmatched) = filterAddressesWithDigits(rules, input)

  println(s"MATCHED:\n$matched\n")
  println(s"UNMATCHED:\n$unmatched\n")
}