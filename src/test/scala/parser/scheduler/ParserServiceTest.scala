package parser.scheduler

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import org.scalatest._
import parser.scheduler.ParserService.AkkaParserActor

class ParserServiceTest extends TestKit(ActorSystem("ActorTest"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll
  with Matchers {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "An AkkaParserActor using TestActorRef " must {
    "should throw IllegalArgumentException when sent unhandled message" in {
      val AkkaParserActorTestRef = TestActorRef(new AkkaParserActor)
      intercept[IllegalArgumentException] {
        AkkaParserActorTestRef.receive("Error: Invalid parsing request! Actors have been killed.")
      }
    }
  }

}
