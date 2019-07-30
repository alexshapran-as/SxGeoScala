package mongo.worker

import configurations.Conf.{confCollectionName, confDatabaseName, confHost}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala._

package object Worker {

  object IpLocation {
    def apply(fromStr: String, from: Long, toStr: String, to: Long, Location: Map[String, Map[String, String]]): IpLocation =
      IpLocation(new String(), fromStr, from, toStr, to, Location)
  }
  case class IpLocation(_id: String, fromStr: String, from: Long, toStr: String, to: Long, Location: Map[String, Map[String, String]])
  import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
  import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
  import org.mongodb.scala.bson.codecs.Macros._
  val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[IpLocation]), DEFAULT_CODEC_REGISTRY)
  val mongo_client: MongoClient = MongoClient(confHost)
  val database: MongoDatabase = mongo_client.getDatabase(confDatabaseName).withCodecRegistry(codecRegistry)
  database.createCollection(confCollectionName)
  val collection: MongoCollection[IpLocation] = database.getCollection(confCollectionName)

}
