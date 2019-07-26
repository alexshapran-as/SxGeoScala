package restapi

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import configurations.Conf.{confInterface, confPort}
import mongo.worker.IpFinder.findEitherIPorErrorMsg
import mongo.worker.Worker._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContextExecutor, Future}

object Receiver {

  private val logger = LoggerFactory.getLogger(getClass)

  implicit val system: ActorSystem = ActorSystem("routing-system")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  def startRouting(): Unit = {

    def errorJson(errorMessage: String): String = pretty(render(Map("Error" -> errorMessage)))

    val route =
      get {
        path("SxGeoScala" / "location" / "get") {
          parameter("ip") {
            case ip: String =>

              val range: Future[Either[String, IpLocation]] = findEitherIPorErrorMsg(ip)

              parameter("show") {

                case city: String  if city.toLowerCase == "city" =>
                  complete(
                    range.collect({
                      case data => data match {
                        case Right(IpLocation(_, _, _, _, _, location)) => HttpEntity(ContentTypes.`application/json`, pretty(render(location("City"))))
                        case Left(errorMsg) => HttpEntity(ContentTypes.`application/json`, errorJson(errorMsg))
                      }
                    })
                  )

                case region: String  if region.toLowerCase == "region" =>
                  complete(
                    range.collect({
                      case data => data match {
                        case Right(IpLocation(_, _, _, _, _, location)) => HttpEntity(ContentTypes.`application/json`, pretty(render(location("City"))))
                        case Left(errorMsg) => HttpEntity(ContentTypes.`application/json`, errorJson(errorMsg))
                      }
                    })
                  )

                case country: String  if country.toLowerCase == "country" =>
                  complete(
                    range.collect({
                      case data => data match {
                        case Right(IpLocation(_, _, _, _, _, location)) => HttpEntity(ContentTypes.`application/json`, pretty(render(location("City"))))
                        case Left(errorMsg) => HttpEntity(ContentTypes.`application/json`, errorJson(errorMsg))
                      }
                    })
                  )

                case _ =>
                  logger.error(s"Error: Bad Mongo DB query")
                  complete(HttpEntity(ContentTypes.`application/json`, errorJson(s"Неправильный запрос")))
              }
          }
        } ~
          path("SxGeoScala" / "location" / "get") {
            parameter("ip") {
              case ip: String =>

                val range: Future[Either[String, IpLocation]] = findEitherIPorErrorMsg(ip)

                complete(
                  range.collect({
                    case data => data match {
                      case Right(IpLocation(_, _, _, _, _, location)) => HttpEntity(ContentTypes.`application/json`, pretty(render(location)))
                      case Left(errorMsg) => HttpEntity(ContentTypes.`application/json`, errorJson(errorMsg))
                    }
                  })
                )
            }
          }
      }

    val bindingFuture = Http().bindAndHandle(route, confInterface, confPort)
    println(s"Server online at http://$confInterface:$confPort/")

  }

}
