import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

val addressGood1 = Address("Veilinghavenkade", Some("9"), None, Some("3521AT"), "Utrecht", "NL")
val addressGood2 = Address("Veilinghavenkade", Some("9"), Some("B"), Some("3521AT"), "Utrecht", "NL")
val addressGood3 = Address("Veilinghavenkade", Some("9B"), Some("9A"), Some("3521AT"), "Utrecht", "NL")

val edge1 = Address("Archipel 26 8-10", None, None, Some("3521AT"), "Utrecht", "NL")
val edge2 = Address("Plein 1988 12", None, None, Some("3521AT"), "Utrecht", "NL")
val edge3 = Address("SINGEL 1940-1945 1B", None, None, Some("3521AT"), "Utrecht", "NL")

val noHouseNumber1 = Address("Voorbeeldadres tweede", None, None, Some("3521AT"), "Utrecht", "NL")
val noHouseNumber2 = Address("Eerste tweede derde", None, None, Some("3521AT"), "Utrecht", "NL")
val noHouseNumber3 = Address("Veilinghavenkade", None, None, Some("3521AT"), "Utrecht", "NL")

val fixed1 = Address("Veilinghavenkade 9", None, None, Some("3521AT"), "Utrecht", "NL")
val fixed2 = Address("Veilinghavenkade 9-10", None, None, Some("3521AT"), "Utrecht", "NL")
val fixed3 = Address("Veilinghavenkade 9 10", None, None, Some("3521AT"), "Utrecht", "NL")
val fixed4 = Address("Veilinghavenkade 9-B", None, None, Some("3521AT"), "Utrecht", "NL")
val fixed5 = Address("Veilinghavenkade 9B", None, None, Some("3521AT"), "Utrecht", "NL")
val fixed6 = Address("Veilinghavenkade 9-10B", None, None, Some("3521AT"), "Utrecht", "NL")
val fixed7 = Address("Veilinghavenkade 9-B2", None, None, Some("3521AT"), "Utrecht", "NL")
val fixed8 = Address("Veilinghavenkade 9 B2", None, None, Some("3521AT"), "Utrecht", "NL")

val otherCountry1 = Address("Voorbeeldadres tweede", None, None, Some("3521AT"), "Utrecht", "CZ")
val otherCountry2 = Address("Eerste tweede derde", None, None, Some("3521AT"), "Utrecht", "DE")
val otherCountry3 = Address("Veilinghavenkade", None, None, Some("3521AT"), "Utrecht", "FR")

val na1 = Address("N/A", None, None, Some("3521AT"), "Utrecht", "NL")
val na2 = Address("N/A", Some("9"), None, Some("3521AT"), "Utrecht", "NL")

class AddressTests extends AnyFunSpec:
  describe("Address tests") {
    it("Already good") {
        assert(AddressFixer(addressGood1).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == addressGood1)
        assert(AddressFixer(addressGood2).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == addressGood2)
        assert(AddressFixer(addressGood3).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == addressGood3)
    }

    it("Edge cases") {
        assert(AddressFixer(edge1).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == edge1)
        assert(AddressFixer(edge2).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == edge2)
        assert(AddressFixer(edge3).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == edge3)
    }

    it("No house number in address") {
        assert(AddressFixer(noHouseNumber1).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == noHouseNumber1)
        assert(AddressFixer(noHouseNumber2).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == noHouseNumber2)
        assert(AddressFixer(noHouseNumber3).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == noHouseNumber3)
    }

    it("Fixed") {
        assert(AddressFixer(fixed1).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == 
          Address("Veilinghavenkade", Some("9"), None, Some("3521AT"), "Utrecht", "NL"))
        assert(AddressFixer(fixed2).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == 
          Address("Veilinghavenkade", Some("9"), Some("10"), Some("3521AT"), "Utrecht", "NL"))
        assert(AddressFixer(fixed3).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == 
          Address("Veilinghavenkade", Some("9"), Some("10"), Some("3521AT"), "Utrecht", "NL"))
        assert(AddressFixer(fixed4).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == 
          Address("Veilinghavenkade", Some("9"), Some("B"), Some("3521AT"), "Utrecht", "NL"))
        assert(AddressFixer(fixed5).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == 
          Address("Veilinghavenkade", Some("9"), Some("B"), Some("3521AT"), "Utrecht", "NL"))
        assert(AddressFixer(fixed6).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix ==
          Address("Veilinghavenkade", Some("9"), Some("10B"), Some("3521AT"), "Utrecht", "NL"))
        assert(AddressFixer(fixed7).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix ==
          Address("Veilinghavenkade", Some("9"), Some("B2"), Some("3521AT"), "Utrecht", "NL"))
        assert(AddressFixer(fixed8).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix ==
          Address("Veilinghavenkade", Some("9"), Some("B2"), Some("3521AT"), "Utrecht", "NL"))
    }

    it("Other country") {
        assert(AddressFixer(otherCountry1).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == otherCountry1)
        assert(AddressFixer(otherCountry2).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == otherCountry2)
        assert(AddressFixer(otherCountry3).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == otherCountry3)
    }

    it("N/A addresses") {
        assert(AddressFixer(na1).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == na1)
        assert(AddressFixer(na2).splitAddressHouseNumber.extractAdditionFromHouseNumber.fix == na2)
    }
  }