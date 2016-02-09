package codegen

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by Leonardo on 06/01/2016.
  */
object SamplePlaySlickGenerator extends PlaySlickCodeGenerator {

  val url = "jdbc:mysql://192.168.0.2/YOUR_DATABASE_HERE?characterEncoding=UTF-8"
  val user = "login"
  val password = "password"

  override val tableClassExtends: Map[String, Seq[String]] = Map(
    "user_brand" -> Seq(), //We don't want to use the defaults for n:m link tables
    "user_category" -> Seq()
  )
  override val caseClassExtends: Map[String, Seq[String]] = Map(
    "user_brand" -> Seq(), //We don't want to use the defaults for n:m link tables
    "user_category" -> Seq()
  )
  override val nestedCaseClassTables: Map[String, Map[String, Seq[String]]] = Map(
    "ad_version" -> Map(
      "Address" -> Seq("addressDistrict", "addressCity", "addressState", "addressZipCode"),
      "ContactInfo" -> Seq("email", "phoneCode", "phoneNumber"),
      "AdTypeInfo" -> Seq("price","adTypeImageQuantity", "adTypeVideoQuantity")
    ),
    "user" -> Map(
      "UserAddress" -> Seq(
        "addressStreet",
        "addressNumber",
        "addressComplement",
        "addressDistrict",
        "addressCity",
        "addressState",
        "addressZipCode"
      )
    )
  )
  def main(args: Array[String]) = {
    val x = codegen.map(
      r =>
        r.writeToFile(
          "slick.driver.MySQLDriver",
          "app",
          "models"
        )
    )
    val unused = Await.ready(x, 5 minutes)
  }
}