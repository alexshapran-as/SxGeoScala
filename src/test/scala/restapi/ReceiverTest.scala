package restapi

import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{render, _}
import org.scalatest._
import restapi.Receiver.startRouting
import scalaj.http.Http

class ReceiverTest extends FunSuite with Matchers {

  startRouting()

  val firstExpectedValue: Map[String, Map[String, String]] =
    Map("Region" -> Map("name_en" -> "California", "name_ru" -> "Калифорния", "iso" -> "US-CA", "id" -> "5332921", "country_seek" -> "5967"),
      "City" -> Map("name_en" -> "Norwalk", "name_ru" -> "Норуолк", "region_seek" -> "9881", "id" -> "5377995", "lon" -> "-118.08172", "country_id" -> "225", "lat" -> "33.90224"),
      "Country" -> Map("name_en" -> "United States", "name_ru" -> "США", "iso" -> "US", "id" -> "225", "lon" -> "556.86", "lat" -> "39.76"))

  val secondExpectedValue: Map[String, Map[String, String]] =
    Map("Region" -> Map("name_en" -> "California", "name_ru" -> "Калифорния", "iso" -> "US-CA", "id" -> "5332921", "country_seek" -> "5967"),
      "City" -> Map("name_en" -> "Cupertino", "name_ru" -> "Купертино", "region_seek" -> "9881", "id" -> "5341145", "lon" -> "-122.03217", "country_id" -> "225", "lat" -> "37.32300"),
      "Country" -> Map("name_en" -> "United States", "name_ru" -> "США", "iso" -> "US", "id" -> "225", "lon" -> "556.86", "lat" -> "39.76")
    )

  def thirdFourthFifthSixthExpectedValues(ip: String): Map[String, String] = Map("Error" -> s"Местоположение с ip = $ip не было найдено в базе данных")

  val seventhExpectedValue: Map[String, String] = Map("Error" -> "Неправильно введен ip адрес: ip = 256.256.1.1")

  val expectedValues: Map[String, String] = Map("28.50.35.214" -> pretty(render(firstExpectedValue)),
                                                "17.171.157.87" -> pretty(render(secondExpectedValue)),
                                                "18.128.64.170" -> pretty(render(thirdFourthFifthSixthExpectedValues("18.128.64.170"))),
                                                "0.0.0.0" -> pretty(render(thirdFourthFifthSixthExpectedValues("0.0.0.0"))),
                                                "10.10.10.10" -> pretty(render(thirdFourthFifthSixthExpectedValues("10.10.10.10"))),
                                                "172.1.2.3" -> pretty(render(thirdFourthFifthSixthExpectedValues("172.1.2.3"))),
                                                "256.256.1.1" -> pretty(render(seventhExpectedValue)))


  test("testStartRouting") {
    val ipstoSearch: Seq[String] = Seq("28.50.35.214", "17.171.157.87", "18.128.64.170", "0.0.0.0", "10.10.10.10", "172.1.2.3", "256.256.1.1")
    ipstoSearch.foreach({ ip =>
      Http("http://127.0.0.1:8080/SxGeoScala/location/get")
        .param("ip",ip)
        .timeout(connTimeoutMs = 10000, readTimeoutMs = 50000)
        .asString.body should be (expectedValues(ip))
    })

  }

}
