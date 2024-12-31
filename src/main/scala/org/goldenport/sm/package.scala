package org.goldenport

/*
 * @since   Jan.  4, 2021
 *  version May. 29, 2021
 *  version Jun. 13, 2021
 *  version Oct. 31, 2021
 *  version Nov. 28, 2021
 *  version Aug. 22, 2022
 * @version Sep.  5, 2024
 * @author  ASAMI, Tomoharu
 */
package object sm {
  val PROP_STMRULE_STATEMACHINE = "statemachine"
  val PROP_STMRULE_EVENT = "event"
  val PROP_STMRULE_STATE = "state"
  val PROP_STMRULE_NAME = "name"
  val PROP_STMRULE_VALUE = "value"
  val PROP_STMRULE_KIND = "kind"
  val PROP_STMRULE_ACTIVITY = "activity"
  val PROP_STMRULE_ENTRY = "entry"
  val PROP_STMRULE_EXIT = "exit"
  val PROP_STMRULE_DO = "do"
  val PROP_STMRULE_EFFECT = "effect"
  val PROP_STMRULE_TRANSITION = "transition"
  val PROP_STMRULE_GUARD = "guard"
  val PROP_STMRULE_TO = "to"

  val PROP_STM_STATEMACHINE = "statemachine"
  val PROP_STM_NAME = "name"
  val PROP_STM_RULE = "rule"

  val PROP_STATE_INIT = "INIT"
  val PROP_STATE_FINAL = "FINAL"
  val PROP_STATE_HISTORY = "HISTORY"

  val STATE_VALUE_INIT = 0
  val STATE_VALUE_FINAL = 999999999
  val STATE_VALUE_UNDEFINED = -1
  val STATE_VALUE_CANCELED = 10
  val STATE_VALUE_SUSPENDED = 20
  val STATE_VALUE_RUNNING = 101
  val STATE_VALUE_CONFIRMING = 102
  val STATE_VALUE_CONFIRMED = 103
  val STATE_VALUE_REJECTED = 201
  val STATE_VALUE_DELIVERING = 501
  val STATE_VALUE_DELIVERED = 502

  val STATE_NAME_INIT = PROP_STATE_INIT
  val STATE_NAME_FINAL = PROP_STATE_FINAL
  val STATE_NAME_CANCELED = "canceled"
  val STATE_NAME_SUSPENDED = "suspended"
  val STATE_NAME_RUNNING = "running"
  val STATE_NAME_CONFIRMING = "confirming"
  val STATE_NAME_CONFIRMED = "confirmed"
  val STATE_NAME_REJECTED = "rejected"
  val STATE_NAME_DELIVERING = "delivering"
  val STATE_NAME_DELIVERED = "delivered"
}
