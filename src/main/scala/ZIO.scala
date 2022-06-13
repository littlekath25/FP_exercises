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

  def filterAddressesWithDigits(regexes: List[Regex], addresses: List[Address]): (List[Address], List[Address]) =
    addresses.partition(address => regexes.exists(_.findFirstIn(address.street) != None))

  val (matched, unmatched) = filterAddressesWithDigits(rules, input)

  println(s"MATCHED:\n$matched\n")
  println(s"UNMATCHED:\n$unmatched\n")
}