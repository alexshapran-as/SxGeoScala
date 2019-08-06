package restapi

import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport
import akka.http.scaladsl.marshalling.Marshaller
import spray.json.DefaultJsonProtocol

import scala.xml.NodeSeq

object XmlHelper {

  trait XmlSupport extends DefaultJsonProtocol with ScalaXmlSupport {

    implicit def marshalItem(obj: XmlRequest): NodeSeq =
      <XmlRequest>
        <_id>
          {obj._id}
        </_id>
        <fromStr>
          {obj.fromStr}
        </fromStr>
        <from>
          {obj.from}
        </from>
        <toStr>
          {obj.toStr}
        </toStr>
        <to>
          {obj.to}
        </to>
        <Location>
          <city>
            <name_en>{obj.Location("Location")("city")("name_en")}</name_en>
            <name_ru>{obj.Location("Location")("city")("name_ru")}</name_ru>
            <region_seek>{obj.Location("Location")("city")("region_seek")}</region_seek>
            <id>{obj.Location("Location")("city")("id")}</id>
            <lon>{obj.Location("Location")("city")("lon")}</lon>
            <country_id>{obj.Location("Location")("city")("country_id")}</country_id>
            <lat>{obj.Location("Location")("city")("lat")}</lat>
          </city>
          <region>
            <name_en>{obj.Location("Location")("region")("name_en")}</name_en>
            <name_ru>{obj.Location("Location")("region")("name_ru")}</name_ru>
            <iso>{obj.Location("Location")("region")("iso")}</iso>
            <id>{obj.Location("Location")("region")("id")}</id>
            <country_seek>{obj.Location("Location")("region")("country_seek")}</country_seek>
          </region>
          <country>
            <name_en>{obj.Location("Location")("country")("name_en")}</name_en>
            <name_ru>{obj.Location("Location")("country")("name_ru")}</name_ru>
            <iso>{obj.Location("Location")("country")("iso")}</iso>
            <id>{obj.Location("Location")("country")("id")}</id>
            <lon>{obj.Location("Location")("country")("lon")}</lon>
            <lat>{obj.Location("Location")("country")("lat")}</lat>
          </country>
        </Location>
      </XmlRequest>

    implicit def xmlFormat: Marshaller[XmlRequest, NodeSeq] = Marshaller.opaque[XmlRequest, NodeSeq](marshalItem)
  }
  final case class XmlRequest(_id: String, fromStr: String, from: Long, toStr: String, to: Long, Location: Map[String, Map[String, Map[String, String]]])

  def	parseXml(xmlItem:	NodeSeq):	XmlRequest	=	{
    def replaseNewlinesAndSpaces(str: String): String = str.replaceAll("\n", "").replaceAll("\\s", "")
    val	_id: String =	replaseNewlinesAndSpaces((xmlItem	\	"_id").text)
    val	fromStr: String =	replaseNewlinesAndSpaces((xmlItem	\	"fromStr").text)
    val from: Long = replaseNewlinesAndSpaces((xmlItem \ "from").text).toLong
    val toStr: String = replaseNewlinesAndSpaces((xmlItem \ "toStr").text)
    val to: Long = replaseNewlinesAndSpaces((xmlItem \ "to").text).toLong

    val labels: Map[String, Seq[String]] = Map("city" -> Seq("name_en", "name_ru", "region_seek", "id", "lon", "country_id", "lat"),
      "region" -> Seq("name_en", "name_ru", "iso", "id", "country_seek"),
      "country" -> Seq("name_en", "name_ru", "iso", "id", "lon", "lat"))
    val locationInfo: Map[String, Map[String, String]] = Map("city" -> labels("city").foldLeft(Map.empty[String, String])((acc, label) =>
      acc ++ Map(label -> replaseNewlinesAndSpaces((xmlItem	\\	"Location" \\ "city" \\ label).text)))) ++
      Map("region" ->  labels("region").foldLeft(Map.empty[String, String])((acc, label) =>
        acc ++ Map(label -> replaseNewlinesAndSpaces((xmlItem	\\	"Location" \\ "region" \\ label).text)))) ++
      Map("country" ->  labels("country").foldLeft(Map.empty[String, String])((acc, label) =>
        acc ++ Map(label -> replaseNewlinesAndSpaces((xmlItem	\\	"Location" \\ "country" \\ label).text))))

    val location: Map[String, Map[String, Map[String, String]]] = Map("Location" -> locationInfo)

    XmlRequest(_id, fromStr, from, toStr, to, location)
  }

}
