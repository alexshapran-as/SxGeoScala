package mongo.worker

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import mongo.worker.Worker.{IpLocation, collection}
import org.json4s.jackson.JsonMethods.render
import org.mongodb.scala.model.Filters.{and, equal, gte, lte}
import org.slf4j.LoggerFactory
import parser.ipbuilder.IpBuilder.getIPstringIPint
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import scala.concurrent.{ExecutionContextExecutor, Future}

object IpFinder {

  private val logger = LoggerFactory.getLogger(getClass)

  implicit val system: ActorSystem = ActorSystem("ipfinder-system")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  def findEitherIPorErrorMsg(iptoFind: String): Future[Either[String, IpLocation]] = {

    val ipStringandInt: (String, Long) = getIPstringIPint(iptoFind)

    collection.find(and(lte("from", ipStringandInt._2), gte("to", ipStringandInt._2))).toFuture()
      .map { data =>
      if (ipStringandInt._1.split('.').map(_.toInt).exists(_ > 255)) Left(s"Неправильно введен ip адрес: ip = $iptoFind")
      else {
        if (data.nonEmpty) Right(data.maxBy(_.from).fromStr)
        else Left(s"Местоположение с ip = $iptoFind не было найдено в базе данных")
      }
    } recover {
      case e: Exception =>
        logger.error(e.toString)
        Left(e.toString)
    } flatMap {
      case Right(ipFound) =>
        collection.find(equal("fromStr", ipFound)).headOption().collect({
          case data => data match {
            case Some(IpLocation(_id, fromStr, from, toStr, to, location)) =>
              logger.info(s"${compact(render(location))} was successfully received from Mongo DB")
              Right(IpLocation(_id, fromStr, from, toStr, to, location))
            case None =>
              logger.error("Error searching in Mongo DB")
              Left("Ошибка поиска в базе данных")

        }
      })
      case Left(errorMsg) =>
        logger.error(errorMsg)
        Future { Left(errorMsg) }
    }

  }
}
