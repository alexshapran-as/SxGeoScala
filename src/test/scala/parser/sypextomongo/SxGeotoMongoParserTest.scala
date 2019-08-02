package parser.sypextomongo

import downloader.Downloader.download
import org.scalatest._
import parser.sypextomongo.SxGeotoMongoParser.Header

class SxGeotoMongoParserTest extends FunSuite with Matchers {

  val expectedValueforTestHeaderParsetoMap: Map[String, String] =Map("number of ranges" -> "4946066",
    "size description of the packing format of the city / region / country" -> "157",
    "blocks in one index entry" -> "2785", "id block size in bytes" -> "3", "parser" -> "3", "encoding" -> "0", "maximum size of country record" -> "147",
    "time of creation" -> "2019-05-31T19:49:56.000+03:00", "version" -> "22", "the size of the directory of countries" -> "9387", "id" -> "SxG",
    "maximum size of a region record" -> "175", "items in the first byte index" -> "224", "maximum size of city record" -> "127", "items in the main index" -> "1775",
    "the size of the directory of cities" -> "2625678", "the size of the directory of regions" -> "109102")

  val expectedValueforTestHeaderParsetoMapforTravis: Map[String, String] =Map("number of ranges" -> "4946066",
    "size description of the packing format of the city / region / country" -> "157",
    "blocks in one index entry" -> "2785", "id block size in bytes" -> "3", "parser" -> "3", "encoding" -> "0", "maximum size of country record" -> "147",
    "time of creation" -> "2019-05-31T19:49:56.000Z", "version" -> "22", "the size of the directory of countries" -> "9387", "id" -> "SxG",
    "maximum size of a region record" -> "175", "items in the first byte index" -> "224", "maximum size of city record" -> "127", "items in the main index" -> "1775",
    "the size of the directory of cities" -> "2625678", "the size of the directory of regions" -> "109102")

  val expectedValueforTestParseDescription: Map[String, Map[String, Int]] = Map("Country packaging format" -> Map("name_en" -> 0, "name_ru" -> 0, "iso" -> 2, "id" -> 1, "lon" -> 2, "lat" -> 2),
    "Region packaging format" -> Map("name_en" -> 0, "name_ru" -> 0, "iso" -> 0, "id" -> 3, "country_seek" -> 2),
    "City packaging format" -> Map("name_en" -> 0, "name_ru" -> 0, "region_seek" -> 3, "id" -> 3, "lon" -> 4, "country_id" -> 1, "lat" -> 4))


  test("testparseHeaderAndparseDescription") {
    val testByteArrayOfSxGeo: Array[Byte] = download()
    val testHeader = Header()
    val testHeaderParsetoMap: Map[String, String] = testHeader.parseHeader(testByteArrayOfSxGeo.take(testHeader.headerSize))()
    val testParseDescription = testHeader.parseDescriptionsofPackagingFormat(testByteArrayOfSxGeo.slice(testHeader.headerSize, testHeader.headerSize + testHeaderParsetoMap("size description of the packing format of the city / region / country").toInt))
    testHeaderParsetoMap should(be (expectedValueforTestHeaderParsetoMap) or be (expectedValueforTestHeaderParsetoMapforTravis))
    testParseDescription should be (expectedValueforTestParseDescription)
  }
}
