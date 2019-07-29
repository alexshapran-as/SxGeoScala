package restapi

import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{render, _}
import org.scalatest._
import restapi.ApiService.RoutingService
import scalaj.http.Http

class ApiServiceTest extends FunSuite with Matchers {

  RoutingService().startRouting()

  val firstExpectedValue: String = pretty(render(Map("success" -> "true")) merge render(Map("result" -> Map("Region" -> Map("name_en" -> "California",
    "name_ru" -> "Калифорния", "iso" -> "US-CA", "id" -> "5332921", "country_seek" -> "5967"),
    "City" -> Map("name_en" -> "Norwalk", "name_ru" -> "Норуолк", "region_seek" -> "9881", "id" -> "5377995",
      "lon" -> "-118.08172", "country_id" -> "225", "lat" -> "33.90224"),
    "Country" -> Map("name_en" -> "United States", "name_ru" -> "США", "iso" -> "US", "id" -> "225",
      "lon" -> "556.86", "lat" -> "39.76")))))


  val secondExpectedValue: String = pretty(render(Map("success" -> "true")) merge
    render(Map("result" -> Map("Region" -> Map("name_en" -> "California", "name_ru" -> "Калифорния", "iso" -> "US-CA", "id" -> "5332921",
      "country_seek" -> "5967"),
      "City" -> Map("name_en" -> "Cupertino", "name_ru" -> "Купертино", "region_seek" -> "9881", "id" -> "5341145", "lon" -> "-122.03217",
        "country_id" -> "225", "lat" -> "37.32300"),
      "Country" -> Map("name_en" -> "United States", "name_ru" -> "США", "iso" -> "US", "id" -> "225", "lon" -> "556.86", "lat" -> "39.76") ))))

  def thirdFourthFifthSixthExpectedValues(ip: String): String = pretty(render(Map("success" -> "false",
                                                                            "Error" -> s"Местоположение с ip = $ip не было найдено в базе данных")))

  val seventhExpectedValue: String = pretty(render(Map("success" -> "false","Error" -> "Неправильно введен ip адрес: ip = 256.256.1.1")))

  val eighthExpectedValue: String = pretty(render(Map("success" -> "false","Error" -> "Отказано в доступе")))

  val expectedValues: Map[String, Map[String, String]] = Map(
    "28.50.35.214" -> Map("expectedValue" -> firstExpectedValue, "signature" -> "BPThmgKm5yKZ9FajqcWa/0pKB4gtsQof5i6belLYIgI="),
    "17.171.157.87" -> Map("expectedValue" -> secondExpectedValue, "signature" -> "ejZA3t8m1BUCKUVOnW1KGFa3o62kDXgA0FHUtofuOqE="),
    "18.128.64.170" -> Map("expectedValue" -> thirdFourthFifthSixthExpectedValues("18.128.64.170"), "signature" -> "xgDalR+58k6FO9bQR8pqX9yL4DGBB0q7j+vLd+Qjlig="),
    "0.0.0.0" -> Map("expectedValue" -> thirdFourthFifthSixthExpectedValues("0.0.0.0"), "signature" -> "BTXBbjsLocfvZqedg4NLOzPCoFdpsoUcUQ4Gzd2TchU="),
    "10.10.10.10" -> Map("expectedValue" -> thirdFourthFifthSixthExpectedValues("10.10.10.10"), "signature" -> "eHNYp1tbMZB2XIV3+RZHvPdIajsMKKDjwA0FZi2h0dU="),
    "172.1.2.3" -> Map("expectedValue" -> thirdFourthFifthSixthExpectedValues("172.1.2.3"), "signature" -> "agYW8360sqlz7QZ3HHVWhTcHyWaBnlQp9uI7n1sgy28="),
    "256.256.1.1" -> Map("expectedValue" -> seventhExpectedValue, "signature" -> "ouCk9c3YrW2HisPVW5wPn2X/lL0uY4NjVgDSZ4Ky86Y="),
    "2.2.1.1" -> Map("expectedValue" -> seventhExpectedValue, "signature" -> "Incorrect Signature")
  )


  test("testStartRouting") {
    val ipstoSearch: Seq[String] = Seq("28.50.35.214", "17.171.157.87", "18.128.64.170", "0.0.0.0", "10.10.10.10", "172.1.2.3", "256.256.1.1")
    ipstoSearch.foreach({ ip =>
      Http("http://127.0.0.1:8080/SxGeoScala/location/find").postData(s"""{"ip":"${ip}","show":""}""")
        .headers( Seq( ("signature", expectedValues(ip)("signature")), ("Content-Type", "application/json") ) )
        .timeout(connTimeoutMs = 10000, readTimeoutMs = 50000)
        .asString.body should be (expectedValues(ip)("expectedValue"))
    })

  }

}
