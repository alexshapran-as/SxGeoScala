package configurations

import com.typesafe.config.{Config, ConfigFactory}

object Conf {
  val conf: Config = ConfigFactory.load("IPtoGeo_App_Configurations")
  val confUrl: String = conf.getString("conf.downloader.url")
  val confCronExpression: String = conf.getString("conf.sheduler.cronExpression")
  val confTimeZone: String = conf.getString("conf.sheduler.timeZone")
  val confHost: String = conf.getString("conf.mongo.host")
  val confDatabaseName: String = conf.getString("conf.mongo.databaseName")
  val confCollectionName: String = conf.getString("conf.mongo.collectionName")
  val confInterface: String = conf.getString("conf.apiservice.interface")
  val confPort: Int = conf.getInt("conf.apiservice.port")
  val confSecretKey: String = conf.getString("conf.apiservice.secretKey")
}
