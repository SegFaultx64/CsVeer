package com.traversalsoftware

import shapeless._
import poly._
import syntax.std.traversable._
import syntax.typeable._
import Nat._

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

  object Int {
    def unapply(s : String) : Option[Int] = try {
      val Pat = "([0-9]+)\\.0+$".r
      s match {
        case Pat(a) => Some(a.toInt)
        case _ => Some(s.toInt)
      }
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }

  object Long {
    def unapply(s : String) : Option[Long] = try {
      val Pat = "([0-9]+)\\.0+$".r
      s match {
        case Pat(a) => Some(a.toLong)
        case _ => Some(s.toLong)
      }
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }

  object Boolean {
    def unapply(s : String) : Option[Boolean] = try {
      val Trueish = "^([1-9][0-9]*)|([1-9][0-9]*\\.0+)|(true)|(TRUE)$".r
      val Falseish = "^(0)|(0\\.0+)|(false)|(FALSE)$".r
      Trueish.findFirstIn(s) match {
        case Some(_) => Some(true)
        case None => {
          Falseish.findFirstIn(s) match {
            case Some(_) => Some(false)
            case None => None
          }
        }
      }
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }

  object Float {
    def unapply(s : String) : Option[Float] = try {
      Some(s.toFloat)
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }

  def compute(s: String, t: String): CsvValue[_] = {
    t match {
      case "Int" => {
        s match {
          case Int(a) => CsvInt(a)
          case _ => CsvString(s)
        }
      }
      case "Long" => {
        s match {
          case Long(a) => CsvLong(a)
          case _ => CsvString(s)
        }
      }
      case "Float" => {
        s match {
          case Float(a) => CsvFloat(a)
          case _ => CsvString(s)
        }
      }
      case "Boolean" => {
        s match {
          case Boolean(a) => CsvBoolean(a)
          case _ => CsvString(s)
        }
      }
      case _ => CsvString(s)
    }
  }

  def rowString(s: String, t: String): List[CsvValue[_]] = {
    val ts: List[String] = t.split(',').toList
    val st: List[String] = s.split(',').toList
    st.zip(ts).map(a => compute(a._1, a._2)).toList
  }

  object choose extends Poly1 {
    implicit def caseA[T, S <% CsvValue[T]] = at[S]{s => s.value}
  }

  def validate[V <: HList](data: List[CsvValue[_]])(implicit fl: shapeless.ops.traversable.FromTraversable[V], mapper: shapeless.ops.hlist.Mapper[choose.type,V]) = {
    (data.toHList[V]).map(_.map(choose))
  }

  def tp[V <: HList](x: Option[V])(implicit tupler: shapeless.ops.hlist.Tupler[V]) = {
    x.map(_.tupled)
  }

  def doAll[V <: HList, X <: HList](data: String, schema : String)(implicit fl: shapeless.ops.traversable.FromTraversable[V], mapper: shapeless.ops.hlist.Mapper[choose.type,V], tupler: shapeless.ops.hlist.Tupler[X]) = {
    val innerData = rowString(data, schema)
    // This is bad. This is a cast so it is not verifiable by the compiler.
    tp((validate[V](innerData).asInstanceOf[Option[X]]))
  }

  trait Rules {
    type Row <: HList
    type RowRaw <: HList

    val fake: Row

    final lazy val schema = fake.toList.map(_.toString).mkString(",")

    def run(data: String)(implicit fl: shapeless.ops.traversable.FromTraversable[Row], mapper: shapeless.ops.hlist.Mapper[choose.type,Row], tupler: shapeless.ops.hlist.Tupler[RowRaw]) = doAll[Row, RowRaw](data, schema)

  }

  trait NiaveMemoRules extends Rules {

    var cache: scala.collection.mutable.Map[String, Option[RowRaw]] = scala.collection.mutable.Map empty

    def runCached(data: String)(implicit fl: shapeless.ops.traversable.FromTraversable[Row], mapper: shapeless.ops.hlist.Mapper[choose.type,Row], tupler: shapeless.ops.hlist.Tupler[NiaveMemoRules.this.RowRaw]) = {
      tp[RowRaw](cache.getOrElseUpdate(data, {
        val innerData = rowString(data, schema)
        validate[Row](innerData).asInstanceOf[Option[RowRaw]]
      }))
    }

  }
}
