package parser.sypextomongo

import parser.ipbuilder.IpBuilder.getIPstringIPint
import downloader.Downloader.download
import mongo.worker.Worker.{IpLocation, collection}
import java.text.SimpleDateFormat
import java.util.Date
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object SxGeotoMongoParser {

  private val logger = LoggerFactory.getLogger(getClass)

  case class Header() {

    val headerSize = 40
    val sizeofItemsinFirstandMainIndexes = 4
    val sizesofItemsinHeader: Seq[(String, Int)] = Seq(("id", 3),("version", 1),("time of creation", 4),("parser", 1),
                                          ("encoding", 1),("items in the first byte index", 1),("items in the main index",2),
                                          ("blocks in one index entry",2),("number of ranges", 4),("id block size in bytes", 1),
                                          ("maximum size of a region record",2),("maximum size of city record",2),
                                          ("the size of the directory of regions",4),("the size of the directory of cities",4),
                                          ("maximum size of country record",2),("the size of the directory of countries",4),
                                          ("size description of the packing format of the city / region / country",2))

    @tailrec
    final def parseHeader(headerBlockofBytes: Array[Byte])(sizesofItemsinHeader: Seq[(String, Int)] = sizesofItemsinHeader,
                                                           parsedHeader: Seq[(String, String)] = Seq()): Map[String, String] =
      (headerBlockofBytes, sizesofItemsinHeader) match {

      case (Array(), _) => parsedHeader.toMap
      case (_, Seq()) => parsedHeader.toMap
      case (headerBlock, ("id", size) :: remainingSizesofItemsinHeader) =>
        parseHeader(headerBlock.drop(size))(remainingSizesofItemsinHeader, parsedHeader :+ ("id", new String(headerBlock.take(size), "UTF-8")))
      case (headerBlock, ("time of creation", size) :: remainingSizesofItemsinHeader) => {
        def unixTimeToDateTime(utime: Int): String = {
          val origin = new DateTime(1970, 1, 1, 0, 0, 0)
          origin.plusSeconds(utime).toString
        }
        parseHeader(headerBlock.drop(size))(remainingSizesofItemsinHeader,
          parsedHeader :+ ("time of creation", unixTimeToDateTime(Integer.parseInt(headerBlock.take(size).map("%02x".format(_)).mkString(""), 16))))
      }
      case (headerBlock, (nameofItem, size) :: remainingSizesofItemsinHeader) if nameofItem == "version" ||
                                                                                 nameofItem == "parser" ||
                                                                                 nameofItem == "encoding" ||
                                                                                 nameofItem == "id block size in bytes" =>
        parseHeader(headerBlock.drop(size))(remainingSizesofItemsinHeader, parsedHeader :+ (nameofItem, headerBlock.take(size).map(_.toInt).mkString("")))
      case (headerBlock, (nameofItem, size) :: remainingSizesofItemsinHeader) =>
        parseHeader(headerBlock.drop(size))(remainingSizesofItemsinHeader,
          parsedHeader :+ (nameofItem, java.lang.Long.decode("0x" + headerBlock.take(size).map("%02x".format(_)).mkString("")).toString))
      case _ =>
        logger.error("Error parsing header of SxGeo DB")
        Map()

    }

    def parseDescriptionsofPackagingFormat(descriptionBlockofBytes: Array[Byte]): Map[String, Map[String, Int]] = {

    val parsedDescriptionsInArrayofItemNameItemSize: Array[Array[Map[String, Int]]] = descriptionBlockofBytes
        .map(byte => if (byte == "00".toByte) ' ' else byte.toChar).mkString("")
        .split(' ').map(packagingFormat => packagingFormat.split('/').map(elem => {
            val sizeAndname = elem.split(':')
            sizeAndname(0) match {
              case size1: String if size1 == "t" || size1 == "T" => Map(sizeAndname(1) -> 1)
              case size2: String if size2 == "s" || size2 == "S" || size2 == "n2" || size2 == "c2" => Map(sizeAndname(1) -> 2)
              case size3: String if size3 == "m" || size3 == "M" => Map(sizeAndname(1) -> 3)
              case size4: String if size4 == "i" || size4 == "I" || size4 == "f" || size4 == "N5" => Map(sizeAndname(1) -> 4)
              case _ => Map(sizeAndname(1) -> 0)
            }}))

      def getDescriptionsInMap(descriptionsInArray: Array[Map[String, Int]], acc: Map[String, Int] = Map()): Map[String, Int] =
        descriptionsInArray.foldLeft(acc)((accMap, descMap) => accMap ++ descMap)

      Map("Country packaging format" -> getDescriptionsInMap(parsedDescriptionsInArrayofItemNameItemSize(0)),
        "Region packaging format" -> getDescriptionsInMap(parsedDescriptionsInArrayofItemNameItemSize(1)),
        "City packaging format" -> getDescriptionsInMap(parsedDescriptionsInArrayofItemNameItemSize(2)))

    }

  }

  case class Offsets(header: Header, headerParsetoMap: Map[String, String]) {

    val firstByteIndexStart: Int = header.headerSize + headerParsetoMap("size description of the packing format of the city / region / country").toInt
    val firstByteIndexEnd: Int = firstByteIndexStart + (headerParsetoMap("items in the first byte index").toInt * header.sizeofItemsinFirstandMainIndexes)

    val mainIndxStart: Int = firstByteIndexEnd
    val mainIndexEnd: Int = mainIndxStart + (headerParsetoMap("items in the main index").toInt * header.sizeofItemsinFirstandMainIndexes)

    val rangesStart: Int = mainIndexEnd
    val rangesEnd: Int = rangesStart + (headerParsetoMap("number of ranges").toInt * (headerParsetoMap("id block size in bytes").toInt + 3))

    val regionsStart: Int = rangesEnd
    val regionsEnd: Int = regionsStart + headerParsetoMap("the size of the directory of regions").toInt

    val countriesStart: Int = regionsEnd
    val countriesEnd: Int = countriesStart + headerParsetoMap("the size of the directory of countries").toInt

    val citiesStart: Int = countriesEnd
    val citiesEnd: Int = citiesStart + headerParsetoMap("the size of the directory of cities").toInt

  }

  case class FirstByteIndex(headerParsetoMap: Map[String, String]) {

    val sizeofElements = 4

    def parseFirstIndexes(firstIndexesBlockofBytes: Array[Byte]): Map[Int, BigInt] = {

      val itemsInFirstByteIndex: Int = headerParsetoMap("items in the first byte index").toInt
      @tailrec
      def go(firstIndexesBlockTemp: Array[Byte])(countOfParsedItems: Int = 0,
                                                 s: Seq[(Int, BigInt)] = Seq()): Map[Int, BigInt] = (firstIndexesBlockTemp, countOfParsedItems) match {
        case (_, count) if count == itemsInFirstByteIndex => s.toMap
        case (Array(), _) => Map()
        case (block, count) =>
          go(block.drop(sizeofElements))(count + 1, s :+ (count, BigInt(java.lang.Long.decode("0x" + block.take(sizeofElements).map("%02x".format(_)).mkString("")))))
      }
      go(firstIndexesBlockofBytes)()

    }

  }

  case class MainIndex(headerParsetoMap: Map[String, String]) {

    val elemsInMainIndex: Int = headerParsetoMap("items in the main index").toInt // количество фрагментов 1775
    val firstIpBlockLength = 4 // размер первого ip 4 байта

    @tailrec
    final def parseFirstIp(firstIpBlockofBytes: Array[Byte])(countOfParsedItems: Int = 0,
                                                             firstIpParsedSeq: Seq[String] = Seq()): Seq[String] = (firstIpBlockofBytes, countOfParsedItems) match {

      case (_, count) if count == elemsInMainIndex => firstIpParsedSeq
      case (Array(), _) => Seq()
      case (block, count) =>
        // Декодирует hex побайтно и получает ip
        @tailrec
        def decodeIP(ipBlockTemp: Array[Byte], ipBlockLength: Int)(firstIp: String = ""): String = (ipBlockTemp, ipBlockLength) match {
          case (_, len) if len <= 0 => firstIp.dropRight(1)
          case (Array(), _) => ""
          case (block, len) => decodeIP(block.drop(1), len - 1)(firstIp + java.lang.Long.decode("0x" + block.take(1).map("%02x".format(_)).mkString("")).toString + ".")
        }
        parseFirstIp(block.drop(firstIpBlockLength))(count + 1, firstIpParsedSeq :+ decodeIP(block.take(firstIpBlockLength), firstIpBlockLength)())

    }

  }

  def parseAll(): Unit = {

    val byteArrayOfSxGeo: Array[Byte] = download()
    if (byteArrayOfSxGeo.isEmpty) {
      logger.error("Error getting downloaded data for SxGeo DB")
    }
    else {
      // Парсинг Заголовка
      val header = Header()
      val headerParsetoMap: Map[String, String] = header.parseHeader(byteArrayOfSxGeo.take(header.headerSize))()
      logger.info(s"Parsed header of SxGeo DB: ${headerParsetoMap}")

      // Парсинг описания формата упаковки страны/региона/города
      val descriptionofPackingFormat: Map[String, Map[String, Int]] = header.parseDescriptionsofPackagingFormat(byteArrayOfSxGeo.slice(header.headerSize,
        header.headerSize + headerParsetoMap("size description of the packing format of the city / region / country").toInt))
      logger.info(s"Parsed description of SxGeo DB: ${descriptionofPackingFormat}")

      val offsets = Offsets(header, headerParsetoMap)

      // Парсинг Индекса первых байт
      val firstIndx = FirstByteIndex(headerParsetoMap)
      val firstIndxParsetoMap: Map[Int, BigInt] = firstIndx.parseFirstIndexes(byteArrayOfSxGeo.slice(offsets.firstByteIndexStart,
        offsets.firstByteIndexStart + offsets.firstByteIndexEnd))
      logger.info(s"Parsed first indexes of SxGeo DB: ${firstIndxParsetoMap}")

      // Парсинг Основного индекса
      val mainIndx = MainIndex(headerParsetoMap)
      val mainIndxParseIp: Seq[String] = mainIndx.parseFirstIp(byteArrayOfSxGeo.slice(offsets.mainIndxStart, offsets.mainIndxStart + offsets.mainIndexEnd))()
      logger.info(s"Parsed main indexes of SxGeo DB: ${mainIndxParseIp}")


      // Парсинг соответствующих первым ip диапазонов с относящимися к ним городами, регионами и странами
      val elemsInMainIndex: Int = headerParsetoMap("items in the main index").toInt // количество первых ip 1775
      val firstIPsize = 4 // размер первого ip (4 байта)
      val idBlockSizeinBytes: Int = headerParsetoMap("id block size in bytes").toInt // размер id блока (3 байта)
      val rangeBlockLength = 3 // размер блока диапазона (3 байта)
      val countofRangesforOneFirstIP: Int = headerParsetoMap("blocks in one index entry").toInt // Количество диапазонов для одного первого ip
      val sizeofRangesforOneFirstIP: Int = countofRangesforOneFirstIP * (idBlockSizeinBytes + rangeBlockLength) // Размер блока диапазонов для одного первого ip в байтах
      val sizeofDirectoryofCountries = headerParsetoMap("the size of the directory of countries").toInt // Размер справочника стран (9387 байт)

      // Размеры блоков с информацией о городах
      val maxSizeofCityStr: Int = headerParsetoMap("maximum size of city record").toInt // Максимальный размер записи города (127  байт)
      val regionseekSizeforCity: Int = descriptionofPackingFormat("City packaging format")("region_seek") // 3 байта
      val countryidSizeforCity: Int = descriptionofPackingFormat("City packaging format")("country_id") // 1 байт
      val idCitySize: Int = descriptionofPackingFormat("City packaging format")("id") // 3 байта
      val latCitySize: Int = descriptionofPackingFormat("City packaging format")("lat") // 4 байта
      val lonCitySize: Int = descriptionofPackingFormat("City packaging format")("lon") // 4 байта
      val countofCityStrs: Int = 2
      val latCityStart: Int = regionseekSizeforCity + countryidSizeforCity + idCitySize
      val latCityEnd: Int = latCityStart + latCitySize
      val lonCityStart: Int = latCityEnd
      val lonCityEnd: Int = lonCityStart + lonCitySize

      // Размеры блоков с информацией о регионах
      val maxSizeofRegionStr: Int = headerParsetoMap("maximum size of a region record").toInt // 175
      val countrySeekSize: Int = descriptionofPackingFormat("Region packaging format")("country_seek") // 2 байта
      val idRegionSize: Int = descriptionofPackingFormat("Region packaging format")("id") // 3 байта
      val countofRegionStrs = 3

      // Размеры блоков с информацией о странах
      val maxSizeofCountryStr: Int = headerParsetoMap("maximum size of country record").toInt // 147
      val idCountrySize: Int = descriptionofPackingFormat("Country packaging format")("id") // 1 байт
      val isoCountrySize: Int = descriptionofPackingFormat("Country packaging format")("iso") // 2 байта
      val latCountrySize: Int = descriptionofPackingFormat("Country packaging format")("lat") // 2 байта
      val lonCountrySize: Int = descriptionofPackingFormat("Country packaging format")("lon") // 2 байта
      val countofCountryStrs: Int = 2
      val latCountryStart: Int = idCountrySize + isoCountrySize
      val latCountryEnd: Int = idCountrySize + isoCountrySize + latCountrySize
      val lonCountryStart: Int = latCountryEnd
      val lonCountryEnd: Int = lonCountryStart + lonCountrySize

      val blockofBytesContainingCities: Array[Byte] = byteArrayOfSxGeo.slice(offsets.citiesStart, offsets.citiesStart + offsets.citiesEnd) // Блок байтов, содержащий города
      val blockofBytesContainigRegions: Array[Byte] = byteArrayOfSxGeo.slice(offsets.regionsStart, offsets.regionsStart + offsets.regionsEnd) // Блок байтов, содержащий регионы
      val blockofBytesContainingCountries: Array[Byte] = byteArrayOfSxGeo.slice(offsets.countriesStart, offsets.countriesStart + offsets.countriesEnd) // Блок байтов, содержащий страны


      @tailrec
      def parseCityStrs(cityStrsBlockofBytes: Array[Byte],
                        countofStrs: Int = 0,
                        cityStrsParsed: Seq[(String, String)] = Seq()): Seq[(String, String)] = (cityStrsBlockofBytes, countofStrs) match {

        case (_, count) if count == countofCityStrs => cityStrsParsed
        case (Array(), _) => Seq()
        case (strsBlock, count) if count == 0 =>
          parseCityStrs(strsBlock.dropWhile(_ != "00".toByte).drop(1),
            count + 1,
            cityStrsParsed :+ ("name_ru", new String(strsBlock.takeWhile(_ != "00".toByte), "UTF-8")))
        case (strsBlock, count) if count == 1 =>
          parseCityStrs(strsBlock.dropWhile(_ != "00".toByte).drop(1),
            count + 1,
            cityStrsParsed :+ ("name_en", new String(strsBlock.takeWhile(_ != "00".toByte), "UTF-8")))

      }

      @tailrec
      def lenofCityBlock(cityBlockofBytes: Array[Byte],
                         length: Int = 0,
                         currentcountofCityStrs: Int = 0): Int = (cityBlockofBytes, currentcountofCityStrs) match {

        case (_, count) if count == countofCityStrs =>  length
        case (Array(), _) => 0
        case (block, count) => lenofCityBlock(block.dropWhile(_ != "00".toByte).drop(1), length + block.takeWhile(_ != "00".toByte).length + 1, count + 1)

      }

      case class City(info: Seq[(String, String)] = Seq()) {
        def toMap: Map[String, String] = info.toMap
        def ++(other: City) = City(info ++ other.info)
        def :+(elem: (String, String)) = City(info :+ elem)
      }

      def parseCities(cityBlockofBytes: Array[Byte]): City = cityBlockofBytes match {

        case Array() => City()
        case block =>
          val latBlockofBytes = block.slice(latCityStart, latCityEnd).reverse
          val latStr = if (latBlockofBytes(0).toInt == -1) "-" + java.lang.Long.decode("0x" + latBlockofBytes.map(x => "%02x".format(~x).replaceAll("ff", "")).mkString("")).toString
                       else java.lang.Long.decode("0x" + latBlockofBytes.map("%02x".format(_)).mkString("")).toString

          val lonBlockofBytes = block.slice(lonCityStart, lonCityEnd).reverse
          val lonStr = if (lonBlockofBytes(0).toInt == -1) "-" + java.lang.Long.decode("0x" + lonBlockofBytes.map(x => "%02x".format(~x).replaceAll("ff", "")).mkString("")).toString
                       else java.lang.Long.decode("0x" + lonBlockofBytes.map("%02x".format(_)).mkString("")).toString

          City(parseCityStrs(block.slice(lonCityEnd, lonCityEnd + lenofCityBlock(block.slice(lonCityEnd, lonCityEnd + maxSizeofCityStr))))) :+
            ("region_seek", java.lang.Long.decode("0x" + block.take(regionseekSizeforCity).reverse.map("%02x".format(_)).mkString("")).toString) :+
            ("country_id", java.lang.Long.decode("0x" + block.slice(regionseekSizeforCity, regionseekSizeforCity + countryidSizeforCity).reverse.map("%02x".format(_)).mkString("")).toString) :+
            ("id", java.lang.Long.decode("0x" + block.slice(regionseekSizeforCity + countryidSizeforCity, regionseekSizeforCity + countryidSizeforCity + idCitySize).reverse.map("%02x".format(_)).mkString("")).toString) :+
            ("lat", latStr.patch(latStr.length - 5, ".", 0)) :+
            ("lon", lonStr.patch(lonStr.length - 5, ".", 0))

      }

      @tailrec
      def parseRegionStrs(regionStrsBlockofBytes: Array[Byte],
                          countofStrs: Int = 0,
                          regionStrsParsed: Seq[(String, String)] = Seq()): Seq[(String, String)] = (regionStrsBlockofBytes, countofStrs) match {

        case (_, count) if count == countofRegionStrs => regionStrsParsed
        case (Array(), _) => Seq()
        case (strsBlock, count) if count == 0 =>
          parseRegionStrs(strsBlock.dropWhile(_ != "00".toByte).drop(1),
            count + 1,
            regionStrsParsed :+ ("name_ru" , new String(strsBlock.takeWhile(_ != "00".toByte), "UTF-8")))
        case (strsBlock, count) if count == 1 =>
          parseRegionStrs(strsBlock.dropWhile(_ != "00".toByte).drop(1),
            count + 1,
            regionStrsParsed :+ ("name_en" , new String(strsBlock.takeWhile(_ != "00".toByte), "UTF-8")))
        case (strsBlock, count) if count == 2 =>
          parseRegionStrs(strsBlock.dropWhile(_ != "00".toByte).drop(1),
            count + 1,
            regionStrsParsed :+ ("iso" , new String(strsBlock.takeWhile(_ != "00".toByte), "UTF-8")))

      }

      @tailrec
      def lenofRegionBlock(regionBlockofBytes: Array[Byte],
                           length: Int = 0,
                           currentcountofRegionStrs: Int = 0): Int = (regionBlockofBytes, currentcountofRegionStrs) match {

        case (_, count) if count == countofRegionStrs =>  length
        case (Array(), _) => 0
        case (block, count) => lenofRegionBlock(block.dropWhile(_ != "00".toByte).drop(1), length + block.takeWhile(_ != "00".toByte).length + 1, count + 1)

      }


      case class Region(info: Seq[(String, String)] = Seq()) {
        def toMap: Map[String, String] = info.toMap
        def ++(other: Region) = Region(info ++ other.info)
        def :+(elem: (String, String)) = Region(info :+ elem)
      }

      def parseRegions(regionBlockofBytes: Array[Byte]): Region = regionBlockofBytes match {

        case Array() => Region()
        case block =>
          Region(parseRegionStrs(block.slice(countrySeekSize + idRegionSize,
            countrySeekSize + idRegionSize + lenofRegionBlock(block.slice(countrySeekSize + idRegionSize,
            countrySeekSize + idRegionSize + maxSizeofRegionStr))))) :+
            ("country_seek", java.lang.Long.decode("0x" + block.take(countrySeekSize).reverse.map("%02x".format(_)).mkString("")).toString) :+
            ("id", java.lang.Long.decode("0x" + block.slice(countrySeekSize, countrySeekSize + idRegionSize).reverse.map("%02x".format(_)).mkString("")).toString)

      }

      @tailrec
      def parseCountryStrs(countryStrsBlockofBytes: Array[Byte],
                           countofStrs: Int = 0,
                           countryStrsParsed: Seq[(String, String)] = Seq()): Seq[(String, String)] = (countryStrsBlockofBytes, countofStrs) match {

        case (_, count) if count == countofCountryStrs => countryStrsParsed
        case (Array(), _) => Seq()
        case (block, count) if count == 0 =>
          parseCountryStrs(block.dropWhile(_ != "00".toByte).drop(1),
            count + 1,
            countryStrsParsed :+ ("name_ru" -> new String(block.takeWhile(_ != "00".toByte), "UTF-8")))
        case (block, count) if count == 1 =>
          parseCountryStrs(block.dropWhile(_ != "00".toByte).drop(1),
            count + 1,
            countryStrsParsed :+ ("name_en" -> new String(block.takeWhile(_ != "00".toByte), "UTF-8")))

      }

      @tailrec
      def lenofCountryBlock(countryBlockofBytes: Array[Byte],
                            length: Int = 0,
                            currentcountofCountryStrs: Int = 0): Int = (countryBlockofBytes, currentcountofCountryStrs) match {

        case (_, count) if count == countofCountryStrs =>  length
        case (Array(), _) => 0
        case (block, count) => lenofCountryBlock(block.dropWhile(_ != "00".toByte).drop(1), length + block.takeWhile(_ != "00".toByte).length + 1, count + 1)

      }


      case class Country(info: Seq[(String, String)] = Seq()) {
        def toMap: Map[String, String] = info.toMap
        def ++(other: Country) = Country(info ++ other.info)
        def :+(elem: (String, String)) = Country(info :+ elem)
      }

      def parseCountries(countryBlockofBytes: Array[Byte]): Country = countryBlockofBytes match {

        case Array() => Country()
        case block =>
          val latBlockofBytes = block.slice(latCountryStart, latCountryEnd).reverse
          val latStr = if (latBlockofBytes(0).toInt == -1) "-" + java.lang.Long.decode("0x" + latBlockofBytes.map(x => "%02x".format(~x).replaceAll("ff", "")).mkString("")).toString
                       else java.lang.Long.decode("0x" + latBlockofBytes.map("%02x".format(_)).mkString("")).toString

          val lonBlockofBytes = block.slice(lonCountryStart, lonCountryEnd).reverse
          val lonStr = if (lonBlockofBytes(0).toInt == -1) "-" + java.lang.Long.decode("0x" + lonBlockofBytes.map(x => "%02x".format(~x).replaceAll("ff", "")).mkString("")).toString
                       else java.lang.Long.decode("0x" + lonBlockofBytes.map("%02x".format(_)).mkString("")).toString

          Country(parseCountryStrs(block.slice(lonCountryEnd, lonCountryEnd + lenofCountryBlock(block.slice(lonCountryEnd, lonCountryEnd + maxSizeofCountryStr)))) :+
            ("id" -> java.lang.Long.decode("0x" + block.take(idCountrySize).reverse.map("%02x".format(_)).mkString("")).toString) :+
            ("iso" -> new String(block.slice(idCountrySize, idCountrySize + isoCountrySize), "UTF-8")) :+
            ("lat" -> latStr.patch(latStr.length - 2, ".", 0)) :+
            ("lon" -> lonStr.patch(lonStr.length - 2, ".", 0)))

      }

      case class IpLocationInfo(ip: String = "", locationInfo: Map[String, Map[String, String]] = Map()) {
        def toMap: Map[String, Map[String, Map[String, String]]] = Map(ip -> locationInfo)
      }

      @tailrec
      def parseRangesID(rangesBlockofBytes: Array[Byte],
                        firstOctetOfIP: Byte,
                        countOfParsedIPsInOneRange: Int = 0,
                        parsedRanges: Seq[IpLocationInfo] = Seq()): Seq[IpLocationInfo] = (rangesBlockofBytes, countOfParsedIPsInOneRange) match {

        case (_, count) if count == countofRangesforOneFirstIP => parsedRanges
        case (Array(), _) => Seq()
        case (block, count) =>
          @tailrec
          def decodeIP(ipBlockTemp: Array[Byte], ipBlockLength: Int)(s: String = ""): String = (ipBlockTemp, ipBlockLength) match {
            case (_, len) if len <= 0 => s.dropRight(1)
            case (Array(), _) => ""
            case (block, len) => decodeIP(ipBlockTemp.drop(1), len - 1)(s + java.lang.Long.decode("0x" + block.take(1).map("%02x".format(_)).mkString("")).toString + ".")
          }
          val id = java.lang.Long.decode("0x" + block.slice(rangeBlockLength, rangeBlockLength + idBlockSizeinBytes).map("%02x".format(_)).mkString(""))
          if (id < sizeofDirectoryofCountries) {
            val countrySeek = id.toInt
            val country: Map[String, String] = parseCountries(blockofBytesContainingCountries.drop(countrySeek)).toMap

            parseRangesID(block.drop(rangeBlockLength + idBlockSizeinBytes),
              firstOctetOfIP,
              count + 1,
              parsedRanges :+ IpLocationInfo(ip = java.lang.Long.decode("0x" + "%02x".format(firstOctetOfIP)).toString + "." + decodeIP(block.take(rangeBlockLength), rangeBlockLength)(),
                locationInfo = Map("country" -> country)))
          }
          else {

            val citySeek: Int = (id - sizeofDirectoryofCountries).toInt
            val city: Map[String, String] = parseCities(blockofBytesContainingCities.drop(citySeek)).toMap
            val regionSeek: Int = city("region_seek").toInt
            val region: Map[String, String] = parseRegions(blockofBytesContainigRegions.drop(regionSeek)).toMap
            val countrySeek: Int = region("country_seek").toInt
            val country: Map[String, String] = parseCountries(blockofBytesContainingCountries.drop(countrySeek)).toMap

            parseRangesID(block.drop(rangeBlockLength + idBlockSizeinBytes),
              firstOctetOfIP,
              count + 1,
              parsedRanges :+ IpLocationInfo(ip = java.lang.Long.decode("0x" + "%02x".format(firstOctetOfIP)).toString + "." + decodeIP(block.take(rangeBlockLength), rangeBlockLength)(),
                locationInfo = Map("city" -> city, "region" -> region, "country" -> country)))
          }

      }

      @tailrec
      def parseBD(mainIndexBlockofBytes: Array[Byte], rangesBlockofBytes: Array[Byte])
                 (countOfParsedElemsInMainIndex: Int = 0,
                  parsedIpLocations: Seq[IpLocationInfo] = Seq()): Seq[IpLocationInfo] = (mainIndexBlockofBytes, rangesBlockofBytes, countOfParsedElemsInMainIndex) match {

        case (_, _, count) if count == elemsInMainIndex => parsedIpLocations
        case (Array(), _, _) => Seq()
        case (Array(), Array(), _) => Seq()
        case (currentMainIndexBlock, currentRangesBlock, count) => {
          val sdf = new SimpleDateFormat("MMM dd,yyyy HH:mm:ss")
          val timeBeforeParsing = new Date(System.currentTimeMillis)
          logger.info(s"For ranges starting with ${currentMainIndexBlock(0)}, time before parsing = ${sdf.format(timeBeforeParsing)}")
          val newparsedIpLocations: Seq[IpLocationInfo] = parseRangesID(currentRangesBlock.take(sizeofRangesforOneFirstIP), currentMainIndexBlock(0))
          val timeAfterParsing = new Date(System.currentTimeMillis)
          logger.info(s"For ranges starting with ${currentMainIndexBlock(0)}, time after parsing = ${sdf.format(timeAfterParsing)}")

          @tailrec
          def addToDB(parsedIpLocations: Seq[IpLocationInfo]): Boolean = parsedIpLocations match {
            case Seq() => true
            case currentParsedIpLocation :: Seq() =>
              val fromIpinRange: (String, Long) = getIPstringIPint(currentParsedIpLocation.ip)
              val toIpinRange: (String, Long) = getIPstringIPint(currentParsedIpLocation.ip.split('.')(0) + ".255.255.255")
              val doctoSavetoDB: IpLocation = IpLocation(fromIpinRange._1, fromIpinRange._2, toIpinRange._1, toIpinRange._2, currentParsedIpLocation.locationInfo)
              collection.insertOne(doctoSavetoDB).toFuture().onComplete {
                case Failure(e) =>
                  logger.error(e.toString + " Error inserting to Mongo DB: ip = " + currentParsedIpLocation.ip + " location = " + currentParsedIpLocation.locationInfo)
                case Success(obj) =>
                  logger.info(s"$obj was successfully inserted to Mongo DB: ID/IP = ${doctoSavetoDB._id} Location = ${doctoSavetoDB.Location}")
              }
              true
            case currentParsedIpLocation :: remainingParsedIpLocations =>
              val fromIpinRange: (String, Long) = getIPstringIPint(currentParsedIpLocation.ip)
              val toIpinRange: (String, Long) = if (remainingParsedIpLocations.head.ip != "") getIPstringIPint(remainingParsedIpLocations.head.ip) else getIPstringIPint(currentParsedIpLocation.ip.split('.')(0) + ".255.255.255")
              val doctoSavetoDB: IpLocation = IpLocation(fromIpinRange._1, fromIpinRange._2, toIpinRange._1, toIpinRange._2, currentParsedIpLocation.locationInfo)
              collection.insertOne(doctoSavetoDB).toFuture().onComplete {
                case Failure(e) =>
                  logger.error(e.toString + " Error inserting to Mongo DB: ip = " + currentParsedIpLocation.ip + " location = " + currentParsedIpLocation.locationInfo)
                case Success(obj) =>
                  logger.info(s"$obj was successfully inserted to Mongo DB: ID/IP = ${doctoSavetoDB._id} Location = ${doctoSavetoDB.Location}")
              }
              addToDB(remainingParsedIpLocations)
            case _ => false
          }
          addToDB(newparsedIpLocations)

          parseBD(currentMainIndexBlock.drop(firstIPsize), currentRangesBlock.drop(sizeofRangesforOneFirstIP))(count + 1, parsedIpLocations ++ newparsedIpLocations)
        }

      }
      parseBD(byteArrayOfSxGeo.slice(offsets.mainIndxStart,
        offsets.mainIndxStart + offsets.mainIndexEnd),
        byteArrayOfSxGeo.slice(offsets.rangesStart,
          offsets.rangesStart + offsets.rangesEnd))()

    }

  }

}
