package authenticator

import configurations.Conf.confSecretKey
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.json4s.{DefaultFormats, JValue}
import org.json4s.jackson.JsonMethods.parse
import org.slf4j.LoggerFactory
import restapi.JsonHelper._
import restapi.XmlHelper._
import scala.xml.NodeSeq

object Authenticator {

  private val logger = LoggerFactory.getLogger(getClass)

  def generateHMAC(preHashString: String): String = {

    val secret: SecretKeySpec = new javax.crypto.spec.SecretKeySpec(confSecretKey.getBytes("UTF-8"), "HmacSHA256")
    val mac: Mac = javax.crypto.Mac.getInstance("HmacSHA256")
    mac.init(secret)
    val result: Array[Byte] = mac.doFinal(preHashString.replaceAll("\n", "").replaceAll("\\s", "").getBytes("UTF-8"))
    new sun.misc.BASE64Encoder().encode(result)

  }

  def checkSignatures(clientSignature: String,
                      paramValues: String): Either[String, JsonRequest] = {

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

  def checkXmlSignatures(clientSignature: String,
                         paramValues: String, xmlDocument: NodeSeq): Either[String, XmlRequest] = {

    if (clientSignature == generateHMAC(paramValues)) {
      Right(parseXml(xmlDocument))
    } else {
      logger.error("Отказано в доступе")
      Left(errorJson("Отказано в доступе"))
    }

  }

}
