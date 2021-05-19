package org.goldenport.context

import java.net.URI
import org.goldenport.value._
import org.goldenport.trace.FutureTrace
import org.goldenport.extension.IRecord

/*
 * @since   Feb. 21, 2021
 *  version Mar.  8, 2021
 * @version Apr. 21, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Effect extends Incident {
  def properties: IRecord
}

object Effect {
  sealed trait Crud extends NamedValueInstance {
  }
  object Crud extends EnumerationClass[Crud] {
    val elements = Vector(Create, Read, Update, Delete)

    case object Create extends Crud {
      val name = "create"
    }
    case object Read extends Crud {
      val name = "read"
    }
    case object Update extends Crud {
      val name = "update"
    }
    case object Delete extends Crud {
      val name = "delete"
    }
  }

  sealed trait Io extends Effect {
  }
  object Io extends {
    sealed trait Storage extends Io {
    }
    object Storage {
      case class File(crud: Crud, uri: URI) extends Storage {
        def properties: IRecord = IRecord.data(
          "kind" -> "io",
          "sub-kind" -> "strage",
          "sub-sub-kind" -> "file",
          "crud" -> crud.name
        )
      }
      object File {
        import Crud._
        def create(p: URI) = File(Create, p)
        def read(p: URI) = File(Read, p)
        def update(p: URI) = File(Update, p)
        def delete(p: URI) = File(Delete, p)
      }
    }
  }

  class FutureEffect(val container: FutureTrace) extends Effect {
    def properties: IRecord = IRecord.data(
      "kind" -> "future"
    )
  }
  object FutureEffect {
    def create(p: FutureTrace): FutureEffect = new FutureEffect(p)
  }
}

case class Effects(
  effects: Vector[Effect] = Vector.empty
) {
}
object Effects {
  val empty = Effects()
}
