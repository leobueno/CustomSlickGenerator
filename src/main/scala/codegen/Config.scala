package codegen

/**
  * Created by Leonardo on 17/12/2015.
  */
trait Config {
  def url : String
  def jdbcDriver = "com.mysql.jdbc.Driver"
  def slickProfile = slick.driver.MySQLDriver
  def user : String
  def password : String
  def excludedTables = Seq()
  def useJodaForTimestamp = true
  def generateJsonFormaters = true
  def defaultTableClassExtends : Seq[String]  = Seq("IdentifiableTable[Long]")
  def defaultCaseClassMutableColumns : Seq[String] = Seq("id")
  def defaultCaseClassExtends : Seq[String] = Seq("Entity[Long]")
  def useCamelCase = true
  def autoIncAsOption = false
  def tableClassExtends: Map[String,Seq[String]] = Map()
  def caseClassExtends: Map[String,Seq[String]] = Map()
  def additionalImports : Seq[String] = Seq()
  def nestedCaseClassTables : Map[String, Map[String,Seq[String]]] = Map()
}