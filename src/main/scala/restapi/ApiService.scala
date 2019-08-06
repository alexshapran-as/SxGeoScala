package restapi

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives
import akka.stream.ActorMaterializer
import authenticator.Authenticator._
import configurations.Conf.{confInterface, confPort}
import mongo.worker.IpFinder.findEitherIPorErrorMsg
import mongo.worker.IpInserter.insertIP
import mongo.worker.Worker.IpLocation
import restapi.JsonHelper._
import restapi.XmlHelper._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.xml._

object ApiService {

  implicit val system: ActorSystem = ActorSystem("routing-system")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  case class RoutingService() extends Directives with JsonSupport with XmlSupport {

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
                        case data => getResponseForJsonRequest(data, jsonRequest.show)
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
          } ~ path("SxGeoScala" / "location" / "set") {
            entity(as[NodeSeq]) { requestParamValues: NodeSeq =>
              optionalHeaderValueByName("signature") {

                case Some(headerSignature) => {
                  val responseXmlOrJsonError: Either[String, Future[String]] = checkXmlSignatures(headerSignature, requestParamValues.toString, requestParamValues).map {
                    xmlRequest =>
                      val range: Future[Either[String, String]] = insertIP(xmlRequest)
                      range.collect {
                        case data => data match {
                          case Right(msg) => msg
                          case Left(errMsg) => errMsg
                        }
                      }
                  }
                  complete(
                    responseXmlOrJsonError match {
                      case Left(errorMsg) => HttpEntity(ContentTypes.`application/json`, errorMsg)
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
