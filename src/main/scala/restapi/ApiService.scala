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
import mongo.worker.Worker.IpLocation
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, JValue}
import org.slf4j.LoggerFactory
import spray.json._

import scala.concurrent.{ExecutionContextExecutor, Future}

object ApiService {

  private val logger = LoggerFactory.getLogger(getClass)

  final case class JsonRequest(ip: String, show: String)
  def errorJson(errorMessage: String): String = pretty(render(Map("success" -> "false", "Error" -> errorMessage)))

  implicit val system: ActorSystem = ActorSystem("routing-system")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
    implicit val orderFormat = jsonFormat2(JsonRequest)
  }

  case class RoutingService() extends Directives with JsonSupport {

    def generateHMAC(preHashString: String): String = {

      val secret: SecretKeySpec = new javax.crypto.spec.SecretKeySpec(confSecretKey.getBytes("UTF-8"), "HmacSHA256")
      val mac: Mac = javax.crypto.Mac.getInstance("HmacSHA256")
      mac.init(secret)
      val result: Array[Byte] = mac.doFinal(preHashString.replaceAll("\n", "").replaceAll("\\s", "").getBytes("UTF-8"))
      new sun.misc.BASE64Encoder().encode(result)

    }

    def checkSignatures(clientSignature: String, paramValues: String): Either[String, JsonRequest] = {

      if (clientSignature == generateHMAC(paramValues)) {
        implicit val formats = DefaultFormats
        val parsedJson: JValue = parse(paramValues)
        val ipParam: String = (parsedJson \ "ip").extract[String]
        val showParam: String = (parsedJson \ "show").extract[String]
        Right(JsonRequest(ipParam, showParam))
      } else {
        logger.error("Отказано в доступе")
        Left(errorJson("Отказано в доступе"))
      }

    }

    def getResponse(ipLocationOrErrorMsg: Either[String, IpLocation], showParametr: String): String = (ipLocationOrErrorMsg, showParametr.toLowerCase) match {

      case (Right(IpLocation(_, _, _, _, _, location)), "") =>
        pretty(render(Map("success" -> "true")) merge render(Map("result" -> location)))
      case (Right(IpLocation(_, _, _, _, _, location)), show) =>
        if (location.contains(show))
          pretty( render(Map("success" -> "true")) merge render( Map("result" -> Map(show -> location(show)))) )
        else
          logger.error("Для данного IP-адреса не найдена информация про ${show}")
          errorJson(s"Для данного IP-адреса не найдена информация про ${show}")
      case (Left(errorMsg), _) =>
        errorJson(errorMsg)

    }

    def startRouting(): Unit = {

      val route =
        post {
            path("SxGeoScala" / "location" / "find") {
              entity(as[String]) { requestParamValues: String =>
                optionalHeaderValueByName("signature") {

                  case Some(headerSignature) => {
                    val responseJsonLocationOrJsonError: Either[String, Future[String]] = checkSignatures(headerSignature, requestParamValues).map {
                      jsonRequest =>
                        val range: Future[Either[String, IpLocation]] = findEitherIPorErrorMsg(jsonRequest.ip)
                        range.collect {
                          case data => getResponse(data, jsonRequest.show)
                        }
                    }
                    complete(
                      responseJsonLocationOrJsonError match {
                        case Left(errorJsonMsg) => HttpEntity(ContentTypes.`application/json`, errorJsonMsg)
                        case Right(response) => response.collect { case data => HttpEntity(ContentTypes.`application/json`, data) }
                      })
                  }
                  case None => complete(HttpEntity(ContentTypes.`application/json`, errorJson("Нет подписи. Отказано в доступе")))

                }
              }
            }
        }

      Http().bindAndHandle(route, confInterface, confPort)
      println(s"Server online at http://$confInterface:$confPort/")

    }

  }

}
