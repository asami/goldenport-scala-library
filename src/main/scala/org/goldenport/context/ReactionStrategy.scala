package org.goldenport.context

import org.goldenport.value._

/*
 * @since   Mar. 15, 2021
 * @version Mar. 26, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait ReactionStrategy extends NamedValueInstance {
}

object ReactionStrategy extends EnumerationClass[ReactionStrategy] {
  val none = NoneReaction

  val elements = Vector(NoneReaction, InputReaction, RetryReaction, EscalateReaction)

  case object NoneReaction extends ReactionStrategy {
    val name = "none"
  }

  case object InputReaction extends ReactionStrategy {
    val name = "input"
  }

  case object RetryReaction extends ReactionStrategy {
    val name = "retry"
  }

  case object EscalateReaction extends ReactionStrategy {
    val name = "escalate"
  }
}
