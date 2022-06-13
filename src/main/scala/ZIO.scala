import scala.io.Source
import scala.util.matching.Regex

case class Address(street: String, house_number: Option[String] = None, house_number_addition: Option[String] = None)

object Address extends App {
  val input: List[Address] =
    List(
      Address("PLEIN 1921"),
      Address("WILLEM 2 STRAAT"),
      Address("boeier 01 19A"),
      Address("Willem 2 straat 19"),
      Address("Veilinghavenkade Tweede Derde 129"),
      Address("Veilinghavenkade Tweede 129B"),
      Address("Veilinghavenkade 129-B"),
      Address("Veilinghavenkade 129 1234"),
      Address("Veilinghavenkade 129-1234"),
      Address("Plein 20 0"),
      Address("Archipel 11"),
      Address("Archipel 12"),
      Address("19 Onzinlaan")
    )

  val rules: List[Regex] =
    Source
      .fromFile("src/main/scala/splitsstraathuisnr.ini")
      .getLines
      .map(line => ("(?i)" + line).r)
      .toList

  // Addresss filter of addresses with a digit in the streetname
  def filterAddressesWithDigits(regexes: List[Regex], addresses: List[Address]): (List[Address], List[Address]) =
    addresses.partition(address => regexes.exists(_.findFirstIn(address.street) != None))

  val (withDigits, withoutDigits) = filterAddressesWithDigits(rules, input)

  // Address filter of the address streetname and the rest
  def splitAddressHouseNumber(addresses: List[Address]): List[Address] =
    addresses.map(address =>
      val splitted: Array[String] = address.street.split("(?=\\d)", 2)
      Address(splitted.head.trim, Some(splitted.tail.mkString)))

  val solved = splitAddressHouseNumber(withoutDigits)

  // Housenumber filter of the house number and house number addition
  def splitHouseNumber(addresses: List[Address]): List[Address] =
    ???

  println(s"MATCHED:\n$withDigits\n")
  println(s"UNMATCHED:\n$withoutDigits\n")
  println(s"SOLVED:\n$solved\n")
}