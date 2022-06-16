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

object Address
{
  implicit val addressWrites: Writes[Address] = (address: Address) =>
    Json.obj(
      "street"                  -> JsString(address.street),
      "house_number"            -> JsString(address.house_number),
      "house_number_addition"   -> JsString(address.house_number_addition),
      "postal_code"             -> JsString(address.postal_code),
      "city"                    -> JsString(address.city),
      "country"                 -> JsString(address.country)
    )

  implicit val addressReades: Reads[Address] = ((JsPath \ "messageVersion ").read[String] and
    (JsPath \ "messageId").read[String] and
    (JsPath \ "terminalId").read[String] and
    (JsPath \ "message").read[String]) { (messageVersion, messageId, terminalId, messages) =>
    Iftmin(messageVersion.toLong, UUID.fromString(messageId), terminalId, messages)
  }
}

final case class AddressFixer(original: Address, rules: List[Regex], street: Option[String] = None, house_number: Option[String] = None, house_number_addition: Option[String] = None)
{
  private def checkAddress: Boolean =
    addressIsInvalid && addressContainsHouseNumber && !addressIsException

  // Check if the address is valid for correction
  private def addressContainsHouseNumber: Boolean =
    original.street.exists(_.isDigit)

  private def addressIsException: Boolean =
    rules.exists(_.findFirstIn(original.street).isDefined)

  // Rules to check if address is valid for correction
  private def addressIsInvalid: Boolean =
    original.house_number.isEmpty &&
    original.street != "N/A" &&
    original.country == "NL"

    // Address filter of the address streetname and the rest
  def splitAddressHouseNumber: AddressFixer =
    if (checkAddress)
      val splitted = original.street.split("(?=\\d)", 2).map(_.trim)
      this.copy(street = Some(splitted.head), house_number = Some(splitted(1)))
    else
      this

    // Housenumber filter of the house number and house number addition
  def extractAdditionFromHouseNumber: AddressFixer =
    if (house_number.exists(_.contains(" ")))
      splitHouseNumberAndAddition(" ")
    else if (house_number.exists(_.contains("-")))
      splitHouseNumberAndAddition("-")
    else if (house_number.exists(_.exists(_.isLetter)))
      splitHouseNumberAndAddition("(?=\\D)")
    else
      this

  // Address splitting of house number and addition
  private def splitHouseNumberAndAddition(delimiter: String): AddressFixer =
    val splitted = house_number.get.split(delimiter, 2)
    this.copy(house_number =  Some(splitted.head), house_number_addition = Some(splitted(1)))

  def fix: Address =
    original.copy(
      street = street.getOrElse(original.street),
      house_number = house_number.orElse(original.house_number),
      house_number_addition = house_number_addition.orElse(original.house_number_addition)
    )
}

object Main extends App {
  val schema = AvroSchema[Address]
  val inputStream = AvroInputStream.data[Address].from(getClass.getResourceAsStream("address-testset-uncompressed.avro")).build(schema)
  val inputIterator = inputStream.iterator
  val input = List.from(inputIterator)
  inputStream.close()
}