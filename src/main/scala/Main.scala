import java.time.ZonedDateTime

import scalikejdbc.config.DBs
import scopt._

object Main extends App {
  DBs.setupAll()

  val parser = new OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[String]('c', "crud")
      .action((x, c) => c.copy(crud = x))
      .text("CRUD is an string property")
    opt[String]('p', "process")
      .action((x, c) => c.copy(process = x))
      .text("process is an string property")
    opt[String]('s', "serial")
      .action((x, c) => c.copy(serial = x))
      .text("serial is an string property")
    opt[String]('b', "bleAddress")
      .action((x, c) => c.copy(bleAddress = x))
      .text("ble address is an string property")
    opt[Int]('n', "ng")
      .action((x, c) => c.copy(ng = x))
      .text("ng is an integer property")
    help("help")
  }
  val config = parser.parse(args, Config())

  case class Config(crud: String = "",
                    process: String = "",
                    serial: String = "",
                    bleAddress: String = "",
                    ng: Int = -1)

  parser.parse(args, Config()) match {

    case Some(config) if config.crud == "read" && config.process == "finishedProductInspection" =>

      val beaconFindBySerialAndBLEAddress = Beacon.findBySerialAndBLEAddress(config.serial, config.bleAddress).getOrElse(None)
      val beaconFindBySerial = Beacon.findBySerial(config.serial).getOrElse(None)
      val beaconFindByBLEAddress = Beacon.findByBLEAddress(config.bleAddress).getOrElse(None)

      if (beaconFindBySerialAndBLEAddress.isDefined) {
        println(true, true)
      } else if (beaconFindBySerial.isDefined) {
        println(true, false)
      } else if (beaconFindByBLEAddress.isDefined) {
        beaconFindByBLEAddress.get.visualInspectionDefectiveAt match {
          case Some(_) => println(false, true, true)
          case None => println(false, true, false)
        }
      } else {
        println(false, false)
      }

    case Some(config) if config.crud == "create" && config.process == "finishedProductInspection" =>
      val now = ZonedDateTime.now
      val beacon = Beacon(None, config.serial, config.bleAddress, config.ng, now, None, None, now, now)
      Beacon.create(beacon)

    case Some(config) if config.crud == "update" && config.process == "finishedProductInspection" =>
      val now = ZonedDateTime.now
      var beacon = Beacon.findBySerialAndBLEAddress(config.serial, config.bleAddress).get.get
      beacon = Beacon(beacon.id, config.serial, config.bleAddress, config.ng, now, None, None, beacon.createAt, now)
      Beacon.update(beacon)

    case Some(config) if config.crud == "update" && config.process == "visualInspection" =>
      val now = ZonedDateTime.now
      var beacon = Beacon.findBySerialAndBLEAddress(config.serial, config.bleAddress).get.get
      beacon = Beacon(beacon.id, config.serial, config.bleAddress, beacon.ng, beacon.visualInspectionDefectiveAt.get, None, Some(now), beacon.createAt, now)
      Beacon.update(beacon)

    case Some(config) if config.crud == "update" && config.process == "packaging" =>
      val now = ZonedDateTime.now
      var beacon = Beacon.findBySerialAndBLEAddress(config.serial, config.bleAddress).get.get
      beacon = Beacon(beacon.id, config.serial, config.bleAddress, beacon.ng, beacon.visualInspectionDefectiveAt.get, Some(now), None, beacon.createAt, now)
      Beacon.update(beacon)

    case Some(config) if config.crud == "read" && config.process == "packaging" =>

      val beaconFindBySerialAndBLEAddress = Beacon.findBySerialAndBLEAddress(config.serial, config.bleAddress).getOrElse(None)
      val beaconFindBySerial = Beacon.findBySerial(config.serial).getOrElse(None)
      val beaconFindByBLEAddress = Beacon.findByBLEAddress(config.bleAddress).getOrElse(None)

      if (beaconFindBySerialAndBLEAddress.isDefined) {
        beaconFindBySerialAndBLEAddress.get.visualInspectionDefectiveAt match {
          case Some(_) => println(true, true, true)
          case None => beaconFindBySerialAndBLEAddress.get.packagingAt match {
            case Some(_) => println(true, true, false, true)
            case None => println(true, true, false, false)
          }
        }
      } else if (beaconFindBySerial.isDefined) {
        beaconFindBySerial.get.visualInspectionDefectiveAt match {
          case Some(_) => println(true, false, true)
          case None => beaconFindBySerial.get.packagingAt match {
            case Some(_) => println(true, false, false, true)
            case None => println(true, false, false, false)
          }
        }
      } else if (beaconFindByBLEAddress.isDefined) {
        beaconFindByBLEAddress.get.visualInspectionDefectiveAt match {
          case Some(_) => println(false, true, true)
          case None => beaconFindByBLEAddress.get.packagingAt match {
            case Some(_) => println(false, true, false, true)
            case None => println(false, true, false, false)
          }
        }
      } else {
        println(false, false)
      }
  }
}