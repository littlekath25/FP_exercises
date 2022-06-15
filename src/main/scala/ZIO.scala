import scala.io.Source
import scala.util.matching.Regex
import org.apache.avro.file.DataFileReader
import org.apache.avro.generic.{GenericDatumReader, GenericRecord}
import org.apache.avro.file.DataFileStream
import java.io.InputStream
import java.io.File
import java.io.FileInputStream
import com.sksamuel.avro4s.AvroInputStream
import com.sksamuel.avro4s.AvroSchema
import scala.jdk.CollectionConverters._

final case class Address(street: String, house_number: Option[String] = None, house_number_addition: Option[String] = None, postal_code: Option[String] = None, city: String, country: String)

def addressIsInvalid(address: Address) : Boolean =
    address.house_number == None &&
    address.street != "N/A" &&
    address.country == "NL"

val rules: List[Regex] =
    Source
      .fromResource("splitsstraathuisnr.ini")
      .getLines
      .map(line => ("(?i)" + line).r)
      .toList

object Address extends App {
  val schema = AvroSchema[Address]
  val inputStream = AvroInputStream.data[Address].from(getClass.getResourceAsStream("address-testset-prod-uncompressed.avro")).build(schema)
  val inputIterator = inputStream.iterator
  val input = List.from(inputIterator).filter(addressIsInvalid) 
  inputStream.close()

  // Addresss filter of addresses with a digit in the streetname
  def filterAddressesWithDigits(rules: List[Regex], addresses: List[Address]): (List[Address], List[Address]) =
    addresses.partition(address => rules.exists(_.findFirstIn(address.street) != None))

  // Address splitting of house number and addition
  def splitHouseNumberAndAddition(originalAddress: Address, houseNumber: String, delimiter: String): Address =
    val splitted = houseNumber.split(delimiter, 2)
    originalAddress.copy(house_number =  Some(splitted.head), house_number_addition = Some(splitted.tail.mkString))

  // Housenumber filter of the house number and house number addition
  def extractAdditionFromHouseNumber(originalAddress: Address, houseNumber: String): Address =
    if (houseNumber.contains(" "))
      splitHouseNumberAndAddition(originalAddress, houseNumber, " ")
    else if (houseNumber.contains("-"))
      splitHouseNumberAndAddition(originalAddress, houseNumber, "-")
    else
      splitHouseNumberAndAddition(originalAddress, houseNumber, "(?=\\D)")

  // check if the address is valid for correction
  def addressContainsHouseNumber(splitted: Array[String]): Boolean =
    splitted.size == 2

  // Address filter of the address streetname and the rest
  def splitAddressHouseNumber(originalAddress: Address, streetName: String, houseNumber: String): Address =
    if (houseNumber.contains(" ", "-") || !houseNumber.forall(_.isDigit))
      extractAdditionFromHouseNumber(originalAddress.copy(street = streetName), houseNumber)
    else
      originalAddress.copy(street = streetName, house_number = Some(houseNumber))

  def fixAddress(address: Address): Address =
    val splitted = address.street.split("(?=\\d)", 2).map(_.trim)
    if (addressContainsHouseNumber(splitted))
      splitAddressHouseNumber(address, splitted.head, splitted(1))
    else
      address

  val (withDigits, withoutDigits) = filterAddressesWithDigits(rules, input)
  println("BEFORE: \n" + withoutDigits.take(20).mkString("\n"))
  println("AFTER: \n" + withoutDigits.map(fixAddress).take(20).mkString("\n"))
  // println(fixAddress(Address("Veilinghavenkade 9B", None, None, Some("3521AT"), "Utrecht", "NL")))
}