package downloader

import configurations.Conf.confUrl
import java.io.InputStream
import java.net.{HttpURLConnection, URL}
import java.util.zip.ZipInputStream
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Downloader {

  private val logger = LoggerFactory.getLogger(getClass)

  def download(): Array[Byte] = {
    val url: URL = new URL(confUrl)
    try {
      val connection: HttpURLConnection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      val is: InputStream = connection.getInputStream
      val zis: ZipInputStream = new ZipInputStream(is)

      zis.getNextEntry match {
        case null =>
          logger.error(s"ERROR downloading SxGeo dat file!")
          Array()
        case entry =>
          @tailrec
          def getArray(el: Int, buf: ListBuffer[Byte]): Array[Byte] = el match {
            case -1 =>
              is.close()
              zis.close()
              connection.disconnect()
              buf.toArray
            case x =>
              buf.append(x.byteValue())
              getArray(zis.read(), buf)
          }
          getArray(zis.read(), ListBuffer[Byte]())
      }
    }
    catch { case e: Exception =>
      logger.error(e.toString)
      Array()
    }
  }

}
