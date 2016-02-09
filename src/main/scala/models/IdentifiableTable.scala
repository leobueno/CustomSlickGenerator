package models

import slick.driver.MySQLDriver.api._

/**
  * Created by Felipe on 15/12/2015.
  */
trait IdentifiableTable[PK] {

  def id: Rep[PK]
}
