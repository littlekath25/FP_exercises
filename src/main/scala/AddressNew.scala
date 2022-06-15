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

final case class AddressFixer(original: Address, street: String, house_number: Option[String] = None, house_number_addition: Option[String] = None, postal_code: Option[String] = None, city: String, country: String)
{
  def checkAddress: Boolean =
    addressIsInvalid && addressContainsHouseNumber

  // Check if the address is valid for correction
  private def addressContainsHouseNumber: Boolean =
    street.exists(_.isDigit)

  // Rules to check if address is valid for correction
  private def addressIsInvalid : Boolean =
    original.house_number == None &&
    original.street != "N/A" &&
    original.country == "NL"

    // Address filter of the address streetname and the rest
  def splitAddressHouseNumber: AddressFixer =
    val splitted = street.split("(?=\\d)", 2).map(_.trim)
    val streetName = splitted.head
    val houseNumber = splitted(1)
    this.copy(street = streetName, house_number = Some(houseNumber))

    // Housenumber filter of the house number and house number addition
  def extractAdditionFromHouseNumber: AddressFixer =
    if (house_number.contains(" "))
      splitHouseNumberAndAddition(" ")
    else if (house_number.contains("-"))
      splitHouseNumberAndAddition("-")
    else if (house_number.get.exists(_.isLetter))
      splitHouseNumberAndAddition("(?=\\D)")
    else
      this

  // Address splitting of house number and addition
  private def splitHouseNumberAndAddition(delimiter: String): AddressFixer =
    val splitted = house_number.get.split(delimiter, 2)
    this.copy(house_number =  Some(splitted.head), house_number_addition = Some(splitted(1)))

  def fix: Address =
    original.copy(
      street = street,
      house_number = house_number,
      house_number_addition = house_number_addition,
      postal_code = postal_code,
      city = city,
      country = country)
}

object Main extends App {
  val rules: List[Regex] =
      Source
        .fromResource("splitsstraathuisnr.ini")
        .getLines
        .map(line => ("(?i)" + line).r)
        .toList

  val schema = AvroSchema[Address]
  val inputStream = AvroInputStream.data[Address].from(getClass.getResourceAsStream("address-testset-prod-uncompressed.avro")).build(schema)
  val inputIterator = inputStream.iterator
  val input = List.from(inputIterator)
  inputStream.close()

  // Addresss filter of addresses with a digit in the streetname
  def filterAddressesWithDigits(rules: List[Regex], addresses: List[Address]): (List[Address], List[Address]) =
    addresses.partition(address => rules.exists(_.findFirstIn(address.street) != None))

  val (withDigits, withoutDigits) = filterAddressesWithDigits(rules, input)
  val testAddress = withoutDigits.head
  val solved = AddressFixer(testAddress, testAddress.street, None, None, testAddress.postal_code, testAddress.city, testAddress.country)

  println(s"result: ${solved.checkAddress.splitAddressHouseNumber.extractAdditionFromHouseNumber}")

}