package restapi

import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{render, _}
import org.scalatest._
import restapi.ApiService.RoutingService
import scalaj.http.Http

class ApiServiceTest extends FunSuite with Matchers {

  RoutingService().startRouting()

  val firstExpectedValue: String = pretty(render(Map("success" -> "true")) merge render(Map("result" -> Map("region" -> Map("name_en" -> "California",
    "name_ru" -> "Калифорния", "iso" -> "US-CA", "id" -> "5332921", "country_seek" -> "5967"),
    "city" -> Map("name_en" -> "Norwalk", "name_ru" -> "Норуолк", "region_seek" -> "9881", "id" -> "5377995",
      "lon" -> "-118.08172", "country_id" -> "225", "lat" -> "33.90224"),
    "country" -> Map("name_en" -> "United States", "name_ru" -> "США", "iso" -> "US", "id" -> "225",
      "lon" -> "556.86", "lat" -> "39.76")))))


  val secondExpectedValue: String = pretty(render(Map("success" -> "true")) merge
    render(Map("result" -> Map("region" -> Map("name_en" -> "California", "name_ru" -> "Калифорния", "iso" -> "US-CA", "id" -> "5332921",
      "country_seek" -> "5967"),
      "city" -> Map("name_en" -> "Cupertino", "name_ru" -> "Купертино", "region_seek" -> "9881", "id" -> "5341145", "lon" -> "-122.03217",
        "country_id" -> "225", "lat" -> "37.32300"),
      "country" -> Map("name_en" -> "United States", "name_ru" -> "США", "iso" -> "US", "id" -> "225", "lon" -> "556.86", "lat" -> "39.76") ))))

  def thirdFourthFifthSixthExpectedValues(ip: String): String = pretty(render(Map("success" -> "false",
                                                                            "Error" -> s"Местоположение с ip = $ip не было найдено в базе данных")))

  val seventhExpectedValue: String = pretty(render(Map("success" -> "false","Error" -> "Неправильно введен ip адрес: ip = 256.256.1.1")))

  val eighthExpectedValue: String = pretty(render(Map("success" -> "false","Error" -> "Отказано в доступе")))

  val expectedValues: Map[String, Map[String, String]] = Map(
    "28.50.35.214" -> Map("expectedValue" -> firstExpectedValue, "signature" -> "HaxZU+varWZoxkumM3MB48yvYV42TsP9//2zqJlOU8w="),
    "17.171.157.87" -> Map("expectedValue" -> secondExpectedValue, "signature" -> "oloPaVmOevroouU9Vt0qJy4AvGIOcLPakxXUU53j2to="),
    "18.128.64.170" -> Map("expectedValue" -> thirdFourthFifthSixthExpectedValues("18.128.64.170"), "signature" -> "2DRrYf3tb99+qCQEaB8KY1orVfHo2KnE9cD8G+2p2ig="),
    "0.0.0.0" -> Map("expectedValue" -> thirdFourthFifthSixthExpectedValues("0.0.0.0"), "signature" -> "kmEYjQtOyEzWjyVWVXc5lnfM+pHX0NV7rn5CyqsQUmA="),
    "10.10.10.10" -> Map("expectedValue" -> thirdFourthFifthSixthExpectedValues("10.10.10.10"), "signature" -> "W94bsQnmqocPpw9kazxmf/2ivC8f/h91EIAz7Nv/AqA="),
    "172.1.2.3" -> Map("expectedValue" -> thirdFourthFifthSixthExpectedValues("172.1.2.3"), "signature" -> "tC73LsELdw/S7dJr3mRFwPxK8fvfGEoIEw+ht5Xb9T8="),
    "256.256.1.1" -> Map("expectedValue" -> seventhExpectedValue, "signature" -> "AT+xSKbgq8El8Pr0SFWMo11Vc/cXYNqjlO8OYFXPZY4="),
    "2.2.1.1" -> Map("expectedValue" -> eighthExpectedValue, "signature" -> "Incorrect Signature")
  )


  test("testStartRouting") {
    val ipstoSearch: Seq[String] = Seq("28.50.35.214", "17.171.157.87", "18.128.64.170", "0.0.0.0", "10.10.10.10", "172.1.2.3", "256.256.1.1", "2.2.1.1")
    ipstoSearch.foreach({ ip =>
      Http("http://127.0.0.1:8080/SxGeoScala/location/find").postData(s"""{"ip":"${ip}","show":""}""")
        .headers( Seq( ("signature", expectedValues(ip)("signature")), ("Content-Type", "application/json") ) )
        .timeout(connTimeoutMs = 10000, readTimeoutMs = 50000)
        .asString.body should be (expectedValues(ip)("expectedValue"))
    })

  }

}
