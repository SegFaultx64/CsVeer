package com.traversalsoftware.csv

import shapeless._
import poly._
import syntax.std.traversable._
import syntax.typeable._
import Nat._
import Parser._

// Play Json imports
import play.api.libs.json._

object CsVeer {

  abstract class CsvValue[T](val value: T) {
    def toJson: JsValue
  }

  case class CsvString(override val value: String) extends CsvValue[String](value) {
    def toJson = Json.toJson(value)
  }

  case class CsvInt(override val value: Int) extends CsvValue[Int](value) {
    def toJson = Json.toJson(value)
  }

  case class CsvLong(override val value: Long) extends CsvValue[Long](value) {
    def toJson = Json.toJson(value)
  }

  case class CsvFloat(override val value: Float) extends CsvValue[Float](value) {
    def toJson = Json.toJson(value)
  }

  case class CsvBoolean(override val value: Boolean) extends CsvValue[Boolean](value) {
    def toJson = Json.toJson(value)
  }

  class Rules(schemaList: List[String], cellSeperator: Char = ',') {

    object parseSettings extends ParseSettings {
      val schema = schemaList
      val cellSep = cellSeperator
      val rowSep = '\n'
    }

    def run(data: String) = {
      Parser.rowString(data, parseSettings)
    }

    // def run(data: String)(implicit fl: shapeless.ops.traversable.FromTraversable[A], tupler: shapeless.ops.hlist.Tupler[A]) = doAll[A](data, parseSettings)

  }

  // trait NiaveMemoRules extends Rules {

  //   var cache: scala.collection.mutable.Map[String, Option[RowRaw]] = scala.collection.mutable.Map.empty

  //   def runCached(data: String)(implicit fl: shapeless.ops.traversable.FromTraversable[Row], mapper: shapeless.ops.hlist.Mapper[choose.type, Row], tupler: shapeless.ops.hlist.Tupler[NiaveMemoRules.this.RowRaw]) = {
  //     tp[RowRaw](cache.getOrElseUpdate(data, {
  //       val innerData = rowString(data, parseSettings)
  //       validate[Row](innerData).asInstanceOf[Option[RowRaw]]
  //     }))
  //   }

  // }
}
