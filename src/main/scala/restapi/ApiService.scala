package restapi

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives
import akka.stream.ActorMaterializer
import configurations.Conf.{confInterface, confPort, confSecretKey}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import mongo.worker.IpFinder.findEitherIPorErrorMsg
import mongo.worker.Worker._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.slf4j.LoggerFactory
import spray.json._

import scala.concurrent.{ExecutionContextExecutor, Future}

object ApiService {

  final case class JsonRequest(ip: String, show: String)
  def errorJson(errorMessage: String): String = pretty(render(Map("success" -> "false", "Error" -> errorMessage)))

  private val logger = LoggerFactory.getLogger(getClass)

  implicit val system: ActorSystem = ActorSystem("routing-system")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
    implicit val orderFormat = jsonFormat2(JsonRequest)
  }

  case class RoutingService() extends Directives with JsonSupport {

    def generateHMAC(preHashJsonRequest: JsonRequest): String = {

      val preHashString: String = "ip=" + preHashJsonRequest.ip.toLowerCase() + "&show=" + preHashJsonRequest.show.toLowerCase()
      val secret: SecretKeySpec = new javax.crypto.spec.SecretKeySpec(confSecretKey.getBytes("UTF-8"), "HmacSHA256")
      val mac: Mac = javax.crypto.Mac.getInstance("HmacSHA256")
      mac.init(secret)
      val result: Array[Byte] = mac.doFinal(preHashString.getBytes("UTF-8"))
      new sun.misc.BASE64Encoder().encode(result)

    }

    def startRouting(): Unit = {

      val route =
        post {
            path("SxGeoScala" / "location" / "find") {
              entity(as[JsonRequest]) { requestJsonValue: JsonRequest =>
                extractRequest { requestHeaderValues =>

                  if ( requestHeaderValues.getHeader("signature").get().value() == generateHMAC(requestJsonValue) ) {

                    val range: Future[Either[String, IpLocation]] = findEitherIPorErrorMsg(requestJsonValue.ip)

                    complete(
                      range.collect({
                        case data => data match {

                          case Right(IpLocation(_, _, _, _, _, location)) => requestJsonValue.show.toLowerCase match {

                            case "city" =>
                              if (location.contains("City"))
                                HttpEntity(ContentTypes.`application/json`, pretty(render(Map("success" -> "true")) merge
                                  render( Map("result" -> Map("City" -> location("City")) ))))
                              else
                                HttpEntity(ContentTypes.`application/json`, errorJson(s"Для IP-адреса ${requestJsonValue.ip} не найдена информация про город"))
                            case "region" =>
                              if (location.contains("Region"))
                                HttpEntity(ContentTypes.`application/json`, pretty(render(Map("success" -> "true")) merge
                                  render( Map("result" -> Map("Region" -> location("Region")) ))))
                              else
                                HttpEntity(ContentTypes.`application/json`, errorJson(s"Для IP-адреса ${requestJsonValue.ip} не найдена информация про регион"))
                            case "country" =>
                              HttpEntity(ContentTypes.`application/json`, pretty(render(Map("success" -> "true")) merge
                                render( Map("result" -> Map("Country" -> location("Country")) ))))
                            case _ =>
                              HttpEntity(ContentTypes.`application/json`, pretty(render(Map("success" -> "true")) merge render(Map("result" -> location))))

                          }

                          case Left(errorMsg) => HttpEntity(ContentTypes.`application/json`, errorJson(errorMsg))

                        }
                      })
                    )
                  } else {
                    logger.error(s"Error: Incorrect Signature")
                    complete(HttpEntity(ContentTypes.`application/json`, errorJson("Отказано в доступе")))
                  }

                }
              }
            }
        }

      Http().bindAndHandle(route, confInterface, confPort)
      println(s"Server online at http://$confInterface:$confPort/")

    }

  }

}
