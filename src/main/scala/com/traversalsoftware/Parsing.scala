package com.traversalsoftware.csv

import CsVeer._

object Parser {

  object Int {
    def unapply(s: String): Option[Int] = try {
      val Pat = "([0-9]+)\\.0+$".r
      s match {
        case Pat(a) ⇒ Some(a.toInt)
        case _      ⇒ Some(s.toInt)
      }
    } catch {
      case _: java.lang.NumberFormatException ⇒ None
    }
  }

  object Long {
    def unapply(s: String): Option[Long] = try {
      val Pat = "([0-9]+)\\.0+$".r
      s match {
        case Pat(a) ⇒ Some(a.toLong)
        case _      ⇒ Some(s.toLong)
      }
    } catch {
      case _: java.lang.NumberFormatException ⇒ None
    }
  }

  object Boolean {
    def unapply(s: String): Option[Boolean] = try {
      val Trueish = "^([1-9][0-9]*)|([1-9][0-9]*\\.0+)|(true)|(TRUE)$".r
      val Falseish = "^(0)|(0\\.0+)|(false)|(FALSE)$".r
      Trueish.findFirstIn(s) match {
        case Some(_) ⇒ Some(true)
        case None ⇒ {
          Falseish.findFirstIn(s) match {
            case Some(_) ⇒ Some(false)
            case None    ⇒ None
          }
        }
      }
    } catch {
      case _: java.lang.NumberFormatException ⇒ None
    }
  }

  object Float {
    def unapply(s: String): Option[Float] = try {
      Some(s.toFloat)
    } catch {
      case _: java.lang.NumberFormatException ⇒ None
    }
  }

  class ValidPair(t: String) {

    def unapply(s: String) = {
      t match {
        case "Integer" ⇒ {
          s match {
            case Int(a) ⇒ Some(CsvInt(a))
            case _      ⇒ None
          }
        }
        case "Long" ⇒ {
          s match {
            case Long(a) ⇒ Some(CsvLong(a))
            case _       ⇒ None
          }
        }
        case "Float" ⇒ {
          s match {
            case Float(a) ⇒ Some(CsvFloat(a))
            case _        ⇒ None
          }
        }
        case "Boolean" ⇒ {
          s match {
            case Boolean(a) ⇒ Some(CsvBoolean(a))
            case _          ⇒ None
          }
        }
        case _ ⇒ Some(CsvString(s))
      }
    }
  }

  def compute(s: String, t: String): Option[CsvValue[_]] = {
    val Valid = new ValidPair(t)
    s match {
      case Valid(v) => Some(v)
      case _ => None
    }
  }

  val insideQuotes = "^\"([^\"]*)\"$".r

  def extractFromQuotes(s: String): String = {
    val stripped = insideQuotes.findFirstMatchIn(s)
    stripped match {
      case Some(a) ⇒ { a.toString.drop(1).dropRight(1) }
      case None    ⇒ s
    }
  }

  def rowString(s: String, p: ParseSettings): List[CsvValue[_]] = {
    val st: List[String] = s.split(p.cellSep).toList.map(extractFromQuotes)
    st.zip(p.schema).map(a ⇒ compute(a._1, a._2)).toList
  }

  trait ParseSettings {
    val cellSep: Char
    val rowSep: Char
    val schema: List[String]
  }

}