package mongo.worker

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import mongo.worker.Worker.{IpLocation, collection}
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{pretty, render}
import org.mongodb.scala.model.Filters.equal
import org.slf4j.LoggerFactory
import restapi.XmlHelper._
import restapi.JsonHelper.errorJson
import scala.concurrent.{ExecutionContextExecutor, Future}

object IpInserter {

  private val logger = LoggerFactory.getLogger(getClass)

  implicit val system: ActorSystem = ActorSystem("ipfinder-system")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  def insertIP(xmlRequest: XmlRequest): Future[Either[String, String]] = {
    collection.find(equal("_id", xmlRequest._id)).toFuture().map {
      data =>
       if (data.nonEmpty) {
         logger.error("Insert error in Mongo DB: this record already exists in the database")
         Left(errorJson("Ошибка вставки в Mongo DB: данная запись уже имеется в базе"))
       } else {
         Right("Возможна вставка записи в Mongo DB")
       }
    } recover {
      case e: Exception =>
        logger.error(e.toString)
        Left(e.toString)
    } flatMap {
      case Right(_) => {
        val ipLocation: IpLocation = IpLocation(xmlRequest._id, xmlRequest.fromStr, xmlRequest.from, xmlRequest.toStr, xmlRequest.to, xmlRequest.Location("Location"))
        collection.insertOne(ipLocation).headOption().collect {
          case data => data match {
            case Some(_) => {
              logger.info(s"Record ${pretty(render(xmlRequest.Location("Location")))} was successfully added to Mongo DB database")
              Right(pretty(render(Map("success" -> "true")) merge render(Map("result" -> xmlRequest.Location("Location")))))
            }
            case None => {
              logger.error("Insert Error in Mongo DB")
              Left(errorJson("Ошибка вставки в Mongo DB"))
            }
          }
        }
      }
      case Left(msg) => Future { Left(msg) }
      }
    }

}
