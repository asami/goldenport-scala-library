package org.goldenport.parser

import scalaz._, Scalaz._
import org.goldenport.exception.RAISE

/*
 * @since   Aug. 20, 2018
 * @version Sep. 29, 2018
 * @author  ASAMI, Tomoharu
 */
case class LogicalBlocks(
  blocks: Vector[LogicalBlock]
) {
  def isEmpty: Boolean = blocks.isEmpty

  def lines: LogicalLines = blocks.foldMap(_.lines)

  def +(rhs: LogicalBlocks) = if (rhs.isEmpty)
    this
  else
    LogicalBlocks(blocks ++ rhs.blocks)

  def :+(rhs: LogicalBlock) = if (rhs.isEmpty)
    this
  else
    LogicalBlocks(blocks :+ rhs)

  def +:(lhs: LogicalBlock) = if (lhs.isEmpty)
    this
  else
    LogicalBlocks(lhs +: blocks)
}

object LogicalBlocks {
  type Transition = (ParseMessageSequence, ParseResult[LogicalBlocks], LogicalBlocksParseState)

  implicit object LogicalBlocksMonoid extends Monoid[LogicalBlocks] {
    def zero = empty
    def append(lhs: LogicalBlocks, rhs: => LogicalBlocks) = lhs + rhs
  }

  val empty = LogicalBlocks(Vector.empty)

  def apply(p: LogicalBlock, ps: LogicalBlock*): LogicalBlocks = LogicalBlocks(p +: ps.toVector)

  def create(p: String): LogicalBlocks = apply(LogicalBlock.create(p))

  def parse(in: String): LogicalBlocks = parse(Config.default, in)

  def parse(config: Config, in: String): LogicalBlocks =
    parse(config, LogicalLines.parse(in))

  def parse(in: LogicalLines): LogicalBlocks = parse(Config.default, in)

  def parse(config: Config, in: LogicalLines): LogicalBlocks = {
    val parser = ParseReaderWriterStateClass[Config, LogicalBlocks](config, RootState.init)
    val (messages, result, state) = parser.apply(in)
    result match {
      case ParseSuccess(blocks, _) => blocks
      case ParseFailure(_, _) => RAISE.notImplementedYetDefect
      case EmptyParseResult() => RAISE.notImplementedYetDefect
    }
  }

  def parseDebug(s: String): LogicalBlocks = parse(Config.debug, s)

  case class Config(
    isDebug: Boolean = false
  ) extends ParseConfig {
  }
  object Config {
    val default = Config()
    val debug = Config(true)
  }

  trait LogicalBlocksParseState extends ParseReaderWriterState[Config, LogicalBlocks] {
    def result: LogicalBlocks

    def apply(config: Config, evt: ParseEvent): Transition = {
      if (config.isDebug)
        println(s"IN ($this) <= $evt") // TODO slf4j
      val r = handle_event(config, evt)
      if (config.isDebug)
        println(s"OUT ($this) => $r") // TODO slf4j
      r
    }

    def result(config: Config, p: LogicalBlock): LogicalBlocks = 
      RAISE.notImplementedYetDefect(s"result(${getClass.getSimpleName}, $p)")

    def result(config: Config, p: LogicalBlocks): LogicalBlocks = 
      RAISE.notImplementedYetDefect(s"result(${getClass.getSimpleName}, $p)")

    def addChildState(config: Config, p: Vector[LogicalLines]): LogicalBlocksParseState =
      RAISE.noReachDefect(s"addChildState(${getClass.getSimpleName})")

    def addChildState(config: Config, p: LogicalBlock): LogicalBlocksParseState =
      addChildState(config, LogicalBlocks(p))

    def addChildState(config: Config, p: LogicalBlocks): LogicalBlocksParseState =
      RAISE.noReachDefect(s"addChildState(${getClass.getSimpleName})")

    def addChildTransition(config: Config, p: LogicalBlocks): Transition =
      RAISE.noReachDefect(s"addChildTransition(${getClass.getSimpleName})")

    def addChildTransition(config: Config, p: LogicalBlock, evt: LogicalLineEvent): Transition =
      RAISE.noReachDefect(s"addChildTransition(${getClass.getSimpleName})")

    def addChildEndTransition(config: Config, p: LogicalBlock): Transition = RAISE.noReachDefect(s"addChildEndTransition(${getClass.getSimpleName})")

    protected final def handle_event(config: Config, evt: ParseEvent): Transition =
      evt match {
        case EndEvent => handle_end(config)
        case m: LogicalLineEvent => handle_line(config, m)
        case m: LineEndEvent => RAISE.noReachDefect
        case m: CharEvent => RAISE.noReachDefect
      }

    protected final def handle_end(config: Config): Transition =
      handle_End(config)

    protected def handle_End(config: Config): Transition =
      (ParseMessageSequence.empty, end_Result(config), end_State(config))

    protected def end_Result(config: Config): ParseResult[LogicalBlocks] = 
      RAISE.notImplementedYetDefect(s"end_Result(${getClass.getSimpleName})")

    protected def end_State(config: Config): LogicalBlocksParseState = EndState

    protected final def handle_line(config: Config, evt: LogicalLineEvent): Transition =
      handle_Line(config, evt)

    protected def handle_Line(config: Config, evt: LogicalLineEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, line_State(config, evt))

    protected def line_State(config: Config, evt: LogicalLineEvent): LogicalBlocksParseState = RAISE.notImplementedYetDefect(s"line_State(${getClass.getSimpleName})")
  }

  case class RootState(
    blocks: LogicalBlocks
  ) extends LogicalBlocksParseState {
    def result = blocks

    override def result(config: Config, p: LogicalBlock): LogicalBlocks = blocks :+ p

    override def result(config: Config, p: LogicalBlocks): LogicalBlocks = blocks + p

    override def addChildState(config: Config, p: LogicalBlocks): LogicalBlocksParseState =
      RootState(blocks + p)

    override def line_State(config: Config, evt: LogicalLineEvent) = {
      evt.getSectionTitle.map { title =>
        SectionState(this, title, evt.location)
      }.orElse {
        evt.getSectionUnderline.map { underline =>
          ??? // syntax error
        }
      }.getOrElse {
        InputState(this, Vector(evt.line), LogicalBlocks.empty, evt.location)
      }
    }
  }
  object RootState {
    val init = RootState(LogicalBlocks.empty)
  }

  case class NeutralState(
    parent: LogicalBlocksParseState,
    blocks: LogicalBlocks
  ) extends LogicalBlocksParseState {
    def result = blocks

    override def result(config: Config, p: LogicalBlock): LogicalBlocks =
      parent.result(config, blocks :+ p)

    override def addChildState(config: Config, p: LogicalBlocks): LogicalBlocksParseState =
      RootState(blocks + p)

    override def line_State(config: Config, evt: LogicalLineEvent) = {
      evt.getSectionTitle.map { title =>
        SectionState(this, title, evt.location)
      }.orElse {
        evt.getSectionUnderline.map { underline =>
          ??? // syntax error
        }
      }.getOrElse {
        InputState(this, Vector(evt.line), LogicalBlocks.empty, evt.location)
      }
    }
  }
  object NeutralState {
    // val init = NeutralState(Vector.empty, LogicalBlocks.empty)
  }

  case class InputState(
    parent: LogicalBlocksParseState,
    cs: Vector[LogicalLine],
    blocks: LogicalBlocks,
    location: Option[ParseLocation]
  ) extends LogicalBlocksParseState {
    def result = blocks

    override def addChildState(config: Config, p: LogicalBlocks): LogicalBlocksParseState =
      RootState(blocks + LogicalBlocks(LogicalBlock(cs)) + p)

    override protected def end_Result(config: Config): ParseResult[LogicalBlocks] =
      ParseSuccess(
        parent.addChildState(
          config,
          blocks + LogicalBlocks(LogicalBlock(cs))
        ).result
      )

    override def line_State(config: Config, evt: LogicalLineEvent) =
      evt.getSectionTitle.map { title =>
        SectionState(this, title, evt.location)
      }.orElse {
        evt.getSectionUnderline.map { underline =>
          ???
        }
      }.getOrElse {
        if (evt.isEmptyLine)
          NeutralState(parent, blocks :+ LogicalBlock(cs))
        else
          copy(cs = cs :+ evt.line)
      }
  }
  object InputState {
//    val init = InputState(Vector.empty, LogicalBlocks.empty, ParseLocation.init)
  }

  case class SectionState(
    parent: LogicalBlocksParseState,
    title: LogicalLine.SectionTitle,
    location: Option[ParseLocation],
    cs: Vector[LogicalLine] = Vector.empty,
    blocks: LogicalBlocks = LogicalBlocks.empty
  ) extends LogicalBlocksParseState {
    def result = blocks

    private def _close: LogicalSection = {
      val a = if (cs.isEmpty)
        blocks
      else
        blocks :+ LogicalBlock(cs)
      LogicalSection(title.toI18NElement, a, location)
    }

    private def _flush_state = if (cs.isEmpty)
      this
    else
      copy(cs = Vector.empty, blocks = blocks :+ LogicalBlock(cs))

    override def addChildState(config: Config, p: LogicalBlocks): LogicalBlocksParseState = {
      println(s"addChildState: ($this) <= $p")
      val r = copy(cs = Vector.empty, blocks = (blocks :+ LogicalBlock(cs)) + p)
      println(s"addChildState: ($this) => $r")
      r
    }

    override def result(config: Config, p: LogicalBlock): LogicalBlocks =
      parent.result(config, _close :+ p)

    override protected def end_Result(config: Config): ParseResult[LogicalBlocks] =
      ParseSuccess(parent.result(config, _close))

    def up(config: Config, p: LogicalLine.SectionTitle): LogicalBlocksParseState = parent match {
      case m: SectionState =>
        if (title.level == p.level) {
          println(s"up - same ($this) <= $p")
          val r = m.addChildState(config, _close)
          println(s"up - same ($this) => $r")
          r
        } else if (title.level > p.level) {
          println(s"up - up ($this) <= $p")
          val r = m.up(config, _close, p)
          println(s"up - up ($this) => $r")
          r
        } else {
          RAISE.noReachDefect
        }
      case _ => RAISE.noReachDefect
    }

    def up(config: Config, s: LogicalSection, p: LogicalLine.SectionTitle): LogicalBlocksParseState = parent match {
      case m: SectionState =>
        if (title.level == p.level) {
          println(s"up2 - same ($this) <= $s / $p")
          val r = m.addChildState(config, _close :+ s)
          println(s"up2 - same ($this) => $r")
          r
        } else if (title.level > p.level) {
          println(s"up2 - up ($this) <= $s / $p")
          val r = m.up(config, _close :+ s, p)
          println(s"up2 - up ($this) => $r")
          r
        } else {
          RAISE.noReachDefect
        }
      case _ => RAISE.noReachDefect
    }

    override def line_State(config: Config, evt: LogicalLineEvent) =
      evt.getSectionTitle.map { x =>
        if (title.level == x.level) {
          // println("same")
          SectionState(parent.addChildState(config, _close), x, evt.location)
        } else if (title.level < x.level) {
          // println("down")
          SectionState(_flush_state, x, evt.location)
        } else {
          println(s"up: ($this) <= $evt")
          val r = SectionState(up(config, x), x, evt.location)
          println(s"up: ($this) => $r")
          r
        }
      }.orElse {
        evt.getSectionUnderline.map { underline =>
          ???
        }
      }.getOrElse {
        if (evt.isEmptyLine)
          _flush_state
        else
          copy(cs = cs :+ evt.line)
      }
  }
  object SectionState {
  }

  case object EndState extends LogicalBlocksParseState {
    def result = LogicalBlocks.empty

    override def apply(config: Config, evt: ParseEvent): Transition =
      (ParseMessageSequence.empty, ParseResult.empty, this)
  }
}
