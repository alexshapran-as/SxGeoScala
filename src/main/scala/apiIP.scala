
import org.slf4j.{Logger, LoggerFactory}
import parser.scheduler.ParserService.scheduleInit
import restapi.ApiService.RoutingService

object apiIP {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def main(args: Array[String]): Unit = {

    logger.info("Start service")

    scheduleInit()

    RoutingService().startRouting()

  }
}
