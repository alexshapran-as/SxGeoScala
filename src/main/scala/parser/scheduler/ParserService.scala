package parser.scheduler

import parser.sypextomongo.SxGeotoMongoParser.parseAll
import configurations.Conf.{confCronExpression, confTimeZone}
import java.util.TimeZone
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import org.slf4j.LoggerFactory


object ParserService {

  private val logger = LoggerFactory.getLogger(getClass)

  case class Parse()

  class AkkaParserActor extends  Actor {

    override def receive: Receive = {
      case Parse() => parseAll()
      case _ =>
        logger.error("Error: Invalid parsing request! Actors have been killed.")
        throw new IllegalArgumentException("Error: Invalid parsing request! Actors have been killed.")
        self ! PoisonPill
    }
  }

  def scheduleInit(): Unit = {

    val system: ActorSystem = ActorSystem("parse-system")
    val parserActor: ActorRef = system.actorOf(Props[AkkaParserActor], name="parser")
    val scheduler: QuartzSchedulerExtension = QuartzSchedulerExtension(system)
    val scheduleJobName : String = "JobParser"
    val messageReceiverActor: ActorRef = parserActor
    val messageSentToReceiver = Parse()
    val scheduleCronExpression: String = confCronExpression
    val timeZone: TimeZone = if (confTimeZone == "") TimeZone.getDefault else TimeZone.getTimeZone(confTimeZone)

    try {
      scheduler.createJobSchedule(
        name = scheduleJobName,
        receiver = messageReceiverActor,
        msg = messageSentToReceiver,
        cronExpression = scheduleCronExpression,
        timezone = timeZone)
    } catch {
      case iae: IllegalArgumentException =>
        logger.error(iae.toString)
    }

  }

}
