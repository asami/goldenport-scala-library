package com.asamioffice.goldenport.text

/**
 * @since   Jan.  2, 2011
 *  version Jan. 16, 2011
 *  version Feb. 16, 2012
 * @version Feb. 17, 2013
 * @author  ASAMI, Tomoharu
 */
object UJson {
  val defaultConvert: PartialFunction[Any, String] = {
    case v: Function0[_] => v().asInstanceOf[String]
    case v: Seq[_] => v.mkString("[", ", ", "]") // XXX seq2Json
    case v: Map[_, _] => map2Json(v.asInstanceOf[Map[String, Any]])
    case v: Number => v.toString
    case v: Boolean => v.toString
    case v => '"' + v.toString + '"'
  }

  def map2Json(map: Map[String, Any], convert: PartialFunction[Any, String] = Map.empty): String = {
    tuples2Json(map.toList)
  }

  def optionTuples2Json(seq: Seq[Option[(String, Any)]], convert: PartialFunction[Any, String] = Map.empty): String = {
    tuples2Json(seq.flatten, convert)
  }

  def optionTuples2Json(seq: Option[(String, Any)]*): String = {
    tuples2Json(seq.flatten)
  }

  def tuples2Json(seq: Seq[(String, Any)], convert: PartialFunction[Any, String] = Map.empty): String = {
    seq.flatMap {
      case (key, Some(value)) => Some(key, value)
      case (key, None) => None
      case (key, value) => Some(key, value)
    }.map {
      case (key, value) => "\"" + key + "\": " + jsonString(value, convert)
    }.mkString("{", ", ", "}")
  }

  def tuples2Json(tuples: (String, Any)*): String = {
    tuples2Json(tuples)
  }

  def jsonString(value: Any, convert: PartialFunction[Any, String] = Map.empty) = {
    (convert orElse defaultConvert)(value)
  }

  def seq2JsonSymbol(seq: Seq[(Symbol, Any)]): String = {
    seq.map {
      case (key, value) => "\"" + key.name + "\": " + jsonString(value)
    }.mkString("{", ", ", "}")
  }

  def seq2Json(seq: Seq[(String, Any)]): String = {
    seq.map {
      case (key, value) => "\"" + key + "\": " + jsonString(value)
    }.mkString("{", ", ", "}")
  }
}
