package restapi

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import mongo.worker.Worker.IpLocation
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{pretty, render}
import org.slf4j.LoggerFactory
import spray.json._

object JsonHelper {

  private val logger = LoggerFactory.getLogger(getClass)

  def errorJson(errorMessage: String): String = pretty(render(Map("success" -> "false", "Error" -> errorMessage)))

  trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
    implicit val orderFormat: RootJsonFormat[JsonRequest] = jsonFormat2(JsonRequest)
  }
  final case class JsonRequest(ip: String, show: String)

  def getResponseForJsonRequest(ipLocationOrErrorMsg: Either[String, IpLocation],
                                showParametr: String): String = (ipLocationOrErrorMsg, showParametr.toLowerCase) match {

    case (Right(IpLocation(_, _, _, _, _, location)), "") =>
      pretty(render(Map("success" -> "true")) merge render(Map("result" -> location)))
    case (Right(IpLocation(_, _, _, _, _, location)), show) =>
      if (location.contains(show)) {
        pretty( render(Map("success" -> "true")) merge render( Map("result" -> Map(show -> location(show)))) )
      }
      else {
        logger.error("Для данного IP-адреса не найдена информация про ${show}")
        errorJson(s"Для данного IP-адреса не найдена информация про ${show}")
      }
    case (Left(errorMsg), _) =>
      errorJson(errorMsg)

  }

}
