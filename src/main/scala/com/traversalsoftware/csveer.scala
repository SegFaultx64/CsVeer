package com.traversalsoftware.csv

import shapeless._
import poly._
import syntax.std.traversable._
import syntax.typeable._
import Nat._
import Parser._

object CsVeer {

  class CsvValue[T](val value: T) {
    override def toString = "WHATEVER"
  }

  case class CsvString(override val value: String) extends CsvValue[String](value) {
    override def toString = "String"
  }

  case class CsvInt(override val value: Int) extends CsvValue[Int](value) {
    override def toString = "Int"
  }

  case class CsvLong(override val value: Long) extends CsvValue[Long](value) {
    override def toString = "Long"
  }

  case class CsvFloat(override val value: Float) extends CsvValue[Float](value) {
    override def toString = "Float"
  }

  case class CsvBoolean(override val value: Boolean) extends CsvValue[Boolean](value) {
    override def toString = "Boolean"
  }

  object choose extends Poly1 {
    implicit def caseA[T, S <% CsvValue[T]] = at[S] { s ⇒ s.value }
  }

  def validate[V <: HList](data: List[CsvValue[_]])(implicit fl: shapeless.ops.traversable.FromTraversable[V]) = {
    data.map(choose(_)).toHList[V]
  }

  def tp[V <: HList](x: Option[V])(implicit tupler: shapeless.ops.hlist.Tupler[V]) = {
    x.map(_.tupled)
  }

  def doAll[X <: HList](data: String, parseSettings: ParseSettings)(implicit fl: shapeless.ops.traversable.FromTraversable[X], tupler: shapeless.ops.hlist.Tupler[X]) = {
    val innerData = rowString(data, parseSettings)
    // This is bad. This is a cast so it is not verifiable by the compiler.
    tp((validate[X](innerData).asInstanceOf[Option[X]]))
  }

  trait Rules {
    type Row <: HList

    val fake: Row

    final lazy val schemaComputed = fake.toList.map(_.getClass.getSimpleName)
    val cellSeperator = ','

    object parseSettings extends ParseSettings {
      val schema = schemaComputed
      val cellSep = cellSeperator
      val rowSep = '\n'
    }

    def run(data: String)(implicit fl: shapeless.ops.traversable.FromTraversable[Row], tupler: shapeless.ops.hlist.Tupler[Row]) = doAll[Row](data, parseSettings)

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
