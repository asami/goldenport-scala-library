package org.goldenport.cli.spec

import java.io.File
import com.typesafe.config.{Config => Hocon}
import org.goldenport.context._
import org.goldenport.value._
import org.goldenport.io.InputSource
import org.goldenport.realm.Realm
import org.goldenport.util.AnyUtils

/*
 * @since   Mar.  2, 2025
 * @version Mar. 12, 2025
 * @author  ASAMI, Tomoharu
 */
sealed trait DataType extends NamedValueInstance {
  type InstanceType
  def toInstance(p: Any): InstanceType
  def cInstance(p: Any): Consequence[InstanceType] = Consequence(toInstance(p))
}

object DataType extends EnumerationClass[DataType] {
  val elements = Vector(XString, XFile, XConfig, XRealm)
}

case object XString extends DataType {
  type InstanceType = String
  val name = "string"

  def toInstance(p: Any): String = AnyUtils.toString(p)
}

case object XFile extends DataType {
  type InstanceType = File
  val name = "file"

  def toInstance(p: Any): File = new File(AnyUtils.toString(p))
}

case object XConfig extends DataType {
  type InstanceType = Hocon
  val name = "config"

  def toInstance(p: Any): Hocon = p.asInstanceOf[Hocon]
}

case object XInputSource extends DataType {
  type InstanceType = InputSource
  val name = "inputsource"

  def toInstance(p: Any): InputSource = InputSource.create(p)
}

case object XRealm extends DataType {
  type InstanceType = Realm
  val name = "realm"

  def toInstance(p: Any): Realm = p.asInstanceOf[Realm]
}

trait ExtensionDataType extends DataType {
}
