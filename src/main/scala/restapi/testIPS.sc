import scalaj.http._

val ips = for (i <- 0 to 100) yield (s"${scala.util.Random.nextInt(30)}.${scala.util.Random.nextInt(255)}.${scala.util.Random.nextInt(255)}.${scala.util.Random.nextInt(255)}")

for (i <- 0 to 100) {
  println(s"For ip = ${ips(i)}")
  val response: HttpResponse[String] = Http("http://localhost:8080/SxGeoScala/location/get").param("ip",ips(i)).timeout(connTimeoutMs = 10000, readTimeoutMs = 50000).asString
  println(response.body)
  println(response.code)
  println(response.headers)
  println(response.cookies)
}
