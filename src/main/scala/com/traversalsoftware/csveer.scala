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

  def doAll[V <: HList, X <: HList](data: String)(implicit fl: shapeless.ops.traversable.FromTraversable[V], mapper: shapeless.ops.hlist.Mapper[choose.type,V], tupler: shapeless.ops.hlist.Tupler[X], fake: V) = {
    val innerData = rowString(data, fake.toList.map(_.toString).mkString(","))
    // This is bad. This is a cast so it is not verifiable by the compiler.
    tp((validate[V](innerData).asInstanceOf[Option[X]]))
  }

  // Yeah there must be a better way to do this
  // Macros?

  // implicit val r1i =  CsvInt(0) :: HNil
  // implicit val r1l =  CsvLong(0) :: HNil
  // implicit val r1f =  CsvFloat(0) :: HNil
  // implicit val r1s =  CsvString("") :: HNil
  // implicit val r2ii =  CsvInt(0) :: CsvInt(0) :: HNil
  // implicit val r2il =  CsvInt(0) :: CsvLong(0) :: HNil
  // implicit val r2if =  CsvInt(0) :: CsvFloat(0) :: HNil
  // implicit val r2is =  CsvInt(0) :: CsvString("") :: HNil
  // implicit val r2li =  CsvLong(0) :: CsvInt(0) :: HNil
  // implicit val r2ll =  CsvLong(0) :: CsvLong(0) :: HNil
  // implicit val r2lf =  CsvLong(0) :: CsvFloat(0) :: HNil
  // implicit val r2ls =  CsvLong(0) :: CsvString("") :: HNil
  // implicit val r2fi =  CsvFloat(0) :: CsvInt(0) :: HNil
  // implicit val r2fl =  CsvFloat(0) :: CsvLong(0) :: HNil
  // implicit val r2ff =  CsvFloat(0) :: CsvFloat(0) :: HNil
  // implicit val r2fs =  CsvFloat(0) :: CsvString("") :: HNil
  // implicit val r2si =  CsvString("0") :: CsvInt(0) :: HNil
  // implicit val r2sl =  CsvString("0") :: CsvLong(0) :: HNil
  // implicit val r2sf =  CsvString("0") :: CsvFloat(0) :: HNil
  // implicit val r2ss =  CsvString("0") :: CsvString("") :: HNil
}
