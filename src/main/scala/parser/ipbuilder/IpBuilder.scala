package parser.ipbuilder

object IpBuilder {

  def getIPstringIPint(ip: String): (String, Long) = {
    val ipArrayInt: Array[Int] = ip.split('.').map(_.toInt)
    val iptoInt: Long = (ipArrayInt(0) * 256 * 256 * 256) + (ipArrayInt(1) * 256 * 256) + (ipArrayInt(2) * 256) + ipArrayInt(3)
    (ip, iptoInt)
  }

}
