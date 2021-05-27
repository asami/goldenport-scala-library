package org.goldenport.context

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.util.StringUtils

/*
 * @since   Mar. 12, 2021
 *  version Mar. 27, 2021
 *  version Apr. 29, 2021
 * @version May. 27, 2021
 * @author  ASAMI, Tomoharu
 */
case class DetailCode(
  category: DetailCode.Category, // 1
  site: DetailCode.Site, // 2
  incident: DetailCode.Incident, // 2
  application: Option[DetailCode.ApplicationCode], // 2
  reaction: DetailCode.Reaction // 2
) {
  // 7 or 9
  def code = application.
    map(x =>
      category.code * 100000000 + incident.code * 1000000 + site.code * 10000 + x.code * 100  + reaction.code
    ).getOrElse(
      category.code * 1000000 + incident.code * 10000 + site.code * 100 + reaction.code
    )
  def name = application.map(x =>
    s"${category.name}-${site.name}-${incident.name}-${x.name}-${reaction.name}"
  ).getOrElse(
    s"${category.name}-${site.name}-${incident.name}-${reaction.name}"
  )
}

object DetailCode {
  sealed trait Category {
    def code: Int
    def name: String
  }
  case object Success extends Category {
    def code: Int = 0
    def name: String = "success"
  }
  sealed trait ErrorCategory extends Category {
  }
  sealed trait ArgumentError extends ErrorCategory {
  }
  case object ArgumentValueError extends ArgumentError {
    def code: Int = 1
    def name: String = "argumentvalue"
  }
  case object ArgumentReferenceError extends ArgumentError {
    def code: Int = 2
    def name: String = "argumentreference"
  }
  case object ResultError extends ErrorCategory {
    def code: Int = 3
    def name: String = "result"
  }
  case object StateError extends ErrorCategory {
    def code: Int = 5
    def name: String = "state"
  }
  case object ServiceError extends ErrorCategory {
    def code: Int = 7
    def name: String = "service"
  }

  sealed trait Site {
    def code: Int
    def name: String
  }
  case object SystemSite extends Site {
    def code: Int = 1
    def name: String = "system"
  }
  case object SubSystemSite extends Site {
    def code: Int = 2
    def name: String = "subsystem"
  }
  case object DatabaseSite extends Site {
    def code: Int = 3
    def name: String = "database"
  }
  case object NetworkSite extends Site {
    def code: Int = 4
    def name: String = "network"
  }
  case object FileSite extends Site {
    def code: Int = 11
    def name: String = "file"
  }
  case object EntitySite extends Site {
    def code: Int = 12
    def name: String = "entity"
  }
  case object RuleSite extends Site {
    def code: Int = 13
    def name: String = "rule"
  }
  case object PrincipalSite extends Site {
    def code: Int = 14
    def name: String = "principal"
  }
  case object SessionSite extends Site {
    def code: Int = 15
    def name: String = "session"
  }
  case object PluginSite extends Site {
    def code: Int = 16
    def name: String = "plugin"
  }
  case object OperationSite extends Site {
    def code: Int = 17
    def name: String = "operation"
  }
  case object ServiceSite extends Site {
    def code: Int = 18
    def name: String = "service"
  }
  case object ComponentSite extends Site {
    def code: Int = 19
    def name: String = "component"
  }
  case object ArgumentSite extends Site {
    def code: Int = 20
    def name: String = "argument"
  }
  case object ConfigSite extends Site {
    def code: Int = 21
    def name: String = "config"
  }
  case object ExternalServiceSite extends Site {
    def code: Int = 91
    def name: String = "externalservice"
  }
  case object ExternalApplicationSite extends Site {
    def code: Int = 92
    def name: String = "externalapplication"
  }
  case object ExternalEntitySite extends Site {
    def code: Int = 93
    def name: String = "externalentity"
  }

  sealed trait Incident {
    def code: Int
    def name: String
  }
  case object HighLoad extends Incident {
    def code: Int = 1
    def name: String = "highload"
  }
  case object SyntaxError extends Incident {
    def code: Int = 2
    def name: String = "syntaxerror"
  }
  case object FormatError extends Incident {
    def code: Int = 3
    def name: String = "formaterror"
  }
  case object Invalid extends Incident {
    def code: Int = 4
    def name: String = "invalid"
  }
  case object Duplicate extends Incident {
    def code: Int = 5
    def name: String = "duplicate"
  }
  case object Missing extends Incident {
    def code: Int = 6
    def name: String = "missing"
  }
  case object Redundant extends Incident {
    def code: Int = 7
    def name: String = "redundant"
  }
  case object Immutable extends Incident {
    def code: Int = 8
    def name: String = "immutable"
  }
  case object NotFound extends Incident {
    def code: Int = 9
    def name: String = "notfound"
  }
  case object Existed extends Incident {
    def code: Int = 10
    def name: String = "existed"
  }
  case object Deleted extends Incident {
    def code: Int = 11
    def name: String = "deleted"
  }
  case object TooMany extends Incident {
    def code: Int = 12
    def name: String = "toomany"
  }
  case object Inconsistent extends Incident {
    def code: Int = 13
    def name: String = "inconsistent"
  }
  case object AuthenticationError extends Incident {
    def code: Int = 14
    def name: String = "authenticationerror"
  }
  case object Unauthorized extends Incident {
    def code: Int = 15
    def name: String = "unauthorized"
  }
  case object Conflict extends Incident {
    def code: Int = 16
    def name: String = "conflict"
  }
  case object IoError extends Incident {
    def code: Int = 17
    def name: String = "ioerror"
  }
  case object Malfunction extends Incident {
    def code: Int = 18
    def name: String = "malfunction"
  }
  case object Unsupported extends Incident {
    def code: Int = 19
    def name: String = "unsupported"
  }
  sealed trait Defect extends Incident {
    def name: String = "defect"
  }
  case object LogicDefect extends Defect {
    def code: Int = 91
  }
  case object ConfigurationDefect extends Defect {
    def code: Int = 92
  }
  case object UnsupportedDefect extends Defect {
    def code: Int = 93
  }
  case object NotImplementedYetDefect extends Defect {
    def code: Int = 94
  }
  case object InvariantDefect extends Defect {
    def code: Int = 95
  }
  case object PreConditionStateDefect extends Defect {
    def code: Int = 96
  }
  case object PreConditionDefect extends Defect {
    def code: Int = 97
  }
  case object PostConditionDefect extends Defect {
    def code: Int = 98
  }

  case class Reaction(
    stakeholder: Reaction.Stakeholder,
    action: Reaction.Action
  ) extends Ordered[Reaction] {
    def compare(rhs: Reaction) = {
      val a = stakeholder compare rhs.stakeholder
      if (a == 0)
        action compare rhs.action
      else
        a
    }
    def code: Int = stakeholder.code * 10 + action.code
    def name: String = s"${stakeholder.name}-${action.name}"
  }
  object Reaction {
    implicit val reactionInstance: Order[Reaction] with Show[Reaction] = new Order[Reaction] with Show[Reaction] {
      def order(lhs: Reaction, rhs: Reaction): Ordering = Ordering.fromInt(lhs compare rhs)
    }

    sealed trait Stakeholder extends Ordered[Stakeholder] {
      def code: Int
      def name: String
      def compare(rhs: Stakeholder) = code - rhs.code
    }
    case object Client extends Stakeholder {
      def code: Int = 1
      def name: String = "client"
    }
    case object ApplicationManager extends Stakeholder {
      def code: Int = 2
      def name: String = "appman"
    }
    case object ApplicationAdministrator extends Stakeholder {
      def code: Int = 3
      def name: String = "appadmin"
    }
    case object SystemAdministrator extends Stakeholder {
      def code: Int = 4
      def name: String = "sysadmin"
    }
    case object SystemDeveloper extends Stakeholder {
      def code: Int = 5
      def name: String = "sysdev"
    }

    sealed trait Action extends Ordered[Action] {
      def code: Int
      def name: String
      def compare(rhs: Action) = code - rhs.code
    }
    case object Input extends Action {
      def code: Int = 1
      def name: String = "input"
    }
    case object Retry extends Action {
      def code: Int = 2
      def name: String = "retry"
    }
    case object Config extends Action {
      def code: Int = 3
      def name: String = "config"
    }
    case object Recover extends Action {
      def code: Int = 4
      def name: String = "recover"
    }

    val ClientInput = Reaction(Client, Input)
    val SystemDefect = Reaction(SystemDeveloper, Recover)
  }

  sealed trait ApplicationCode {
    def code: Int
    def name: String
  }
  object ApplicationCode {
    case class ApplicationCodeImpl(code: Int, name: String) extends ApplicationCode

    def apply(code: Int, name: String): ApplicationCode = {
      if (code > 99 || code < 0)
        RAISE.invalidArgumentFault(s"Invalid code: $code")
      if (!StringUtils.isAsciiAlphabetString(name))
        RAISE.invalidArgumentFault(s"Invalid name: $name")
      ApplicationCodeImpl(code, name)
    }
  }

  val Argument = DetailCode(ArgumentValueError, ArgumentSite, Invalid, Reaction.ClientInput)
  val Result = DetailCode(ResultError, OperationSite, LogicDefect, Reaction.SystemDefect)
  val NoReach = DetailCode(ServiceError, OperationSite, LogicDefect, Reaction.SystemDefect)
  val Invariant = DetailCode(ServiceError, OperationSite, InvariantDefect, Reaction.SystemDefect)
  val PreCondition = DetailCode(ServiceError, OperationSite, PreConditionDefect, Reaction.SystemDefect)
  val PreConditionState = DetailCode(ServiceError, OperationSite, PreConditionStateDefect, Reaction.SystemDefect)
  val PostCondition = DetailCode(ServiceError, OperationSite, PostConditionDefect, Reaction.SystemDefect)

  def apply(category: Category, site: Site, incident: Incident, reaction: Reaction): DetailCode =
    DetailCode(category, site, incident, None, reaction)
}
