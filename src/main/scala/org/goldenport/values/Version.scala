package org.goldenport.values

/*
 * @since   Nov. 22, 2016
 *  version Jan. 12, 2017
 *  version Jul. 29, 2017
 *  version Aug. 29, 2017
 * @version Jun.  3, 2020
 * @author  ASAMI, Tomoharu
 */
case class Version(v: String) extends Ordered[Version] {
  import Version._

  lazy val components: List[Component] = Version.components(v)

  override def toString() = v

  lazy val majorNumber: Int = components.headOption.fold(0)(_ match {
    case EmptyComponent => 0
    case NumberComponent(n, _) => n
    case NumberStringComponent(n, _, stage) => n
    case m: StringComponent => 0
  })

  def compare(rhs: Version): Int = {
    @annotation.tailrec
    def go(ls: List[Component], rs: List[Component]): Int = {
      (ls, rs) match {
        case (Nil, Nil) => COMPARE_EQUAL
        case (l, Nil) => COMPARE_GREATER
        case (Nil, r) => COMPARE_LESSER
        case (lh :: lt, rh :: rt) => lh.compare(rh) match {
          case COMPARE_EQUAL => go(lt, rt)
          case x => x
        }
      }
    }
    go(components, rhs.components)
  }

  def isEqual(rhs: Version): Boolean = compare(rhs) == 0
  def isigher(rhs: Version): Boolean  = compare(rhs) > 0
  def isLower(rhs: Version): Boolean  = compare(rhs) < 0
  def isEqualOrHigher(rhs: Version): Boolean  = compare(rhs) >= 0
  def isEqualOrLower(rhs: Version): Boolean  = compare(rhs) <= 0

  def isVolatile: Boolean = components.lastOption.fold(false)(_.isVolatile)
}

object Version {
  val COMPARE_LESSER = -1
  val COMPARE_EQUAL = 0
  val COMPARE_GREATER = 1

  val volatileStages = Set("SNAPSHOT", "LATEST")

  sealed trait Component extends Ordered[Component] {
    def stage: Option[Component]
    def isVolatile: Boolean = stage.fold(false) {
      case StringComponent(s, _) => volatileStages.contains(s)
      case _ => false
    }
  }
  case object EmptyComponent extends Component {
    def compare(rhs: Component) = COMPARE_LESSER
    override def stage = None
  }
  case class NumberComponent(n: Int, stage: Option[Component]) extends Component {
    def compare(rhs: Component): Int = rhs match {
      case NumberComponent(rn, rstage) => _compare(n, rn, stage, rstage)
      case NumberStringComponent(rn, rs, rstage) => n.compare(rn) match {
        case COMPARE_EQUAL => COMPARE_LESSER
        case x => x
      }
      case m: StringComponent => COMPARE_LESSER
      case EmptyComponent => COMPARE_GREATER
    }
  }
  case class NumberStringComponent(n: Int, s: String, stage: Option[Component]) extends Component {
    def compare(rhs: Component): Int = rhs match {
      case NumberComponent(rn, rstage) => n.compare(rn) match {
        case COMPARE_EQUAL => COMPARE_GREATER
        case x => x
      }
      case NumberStringComponent(rn, rs, rstage) => _compare(n, rn, s, rs, stage, rstage)
      case m: StringComponent => COMPARE_LESSER
      case EmptyComponent => COMPARE_GREATER
    }
  }
  case class StringComponent(s: String, stage: Option[Component]) extends Component {
    def compare(rhs: Component): Int = rhs match {
      case NumberComponent(rn, rstage) => COMPARE_GREATER
      case NumberStringComponent(rn, rs, rstage) => COMPARE_GREATER
      case StringComponent(rs, rstage) => _compare(s, rs, stage, rstage)
      case EmptyComponent => COMPARE_GREATER
    }
  }

  private def _compare(
    lhs: Int, rhs: Int, lstage: Option[Component], rstage: Option[Component]
  ): Int =
    lhs.compareTo(rhs) match {
      case COMPARE_EQUAL => _compare(lstage, rstage)
      case x => x
    }

  private def _compare(
    lhs: String, rhs: String, lstage: Option[Component], rstage: Option[Component]
  ): Int =
    lhs.compareTo(rhs) match {
      case COMPARE_EQUAL => _compare(lstage, rstage)
      case x => x
    }

  private def _compare(
    lhs: Int, rhs: Int, ls: String, rs: String, lstage: Option[Component], rstage: Option[Component]
  ): Int =
    lhs.compareTo(rhs) match {
      case COMPARE_EQUAL => ls.compareTo(rs) match {
        case COMPARE_EQUAL => _compare(lstage, rstage)
        case y => y
      }
      case x => x
    }

  private def _compare(
    lstage: Option[Component],
    rstage: Option[Component]
  ) =
    (lstage, rstage) match {
      case (None, None) => COMPARE_EQUAL
      case (Some(l), None) => COMPARE_LESSER
      case (None, Some(r)) => COMPARE_GREATER
      case (Some(l), Some(r)) => l.compare(r)
    }

  def components(v: String): List[Component] = {
    val xs = v.trim.split("[.]")
    for (x <- xs.toList) yield {
      val (a, stage) = x.split("[-]").toList match {
        case Nil => ("", None)
        case x :: Nil => (x, None)
        case x :: xs => (x, Some(StringComponent(xs.mkString("-"), None)))
      }
      a.span(Character.isDigit) match {
        case ("", "") => EmptyComponent
        case (n, "") => NumberComponent(n.toInt, stage)
        case ("", s) => StringComponent(s, stage)
        case (n, s) => NumberStringComponent(n.toInt, s, stage)
      }
    }
  }
}
