package org.goldenport.values

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import java.util.UUID

/*
 * @since   Dec. 26, 2020
 * @version Dec. 26, 2020
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class CompactUuidSpec extends WordSpec with Matchers with GivenWhenThen {
  import Urn._

  "CompactUuid String" should {
    "encode/decode" in {
      val uuid = UUID.randomUUID()
      val encoded = CompactUuid.encode(uuid)
      val decoded = CompactUuid.decode(encoded)
      // println(s"$uuid -> $encoded (${encoded.length})")
      decoded should be(uuid)
    }
    "ca8b02d6-faf9-4e05-9e45-4445f2d54ae8" in {
      val suuid = "ca8b02d6-faf9-4e05-9e45-4445f2d54ae8"
      val scuuid = "6AByeZsHizm8nXoRDPQZUu"
      val uuid = UUID.fromString(suuid)
      val encoded = CompactUuid.encode(uuid)
      encoded should be(scuuid)
      val decoded = CompactUuid.decode(encoded)
      decoded should be(uuid)
    }
    "c2001147-c8fa-4caf-aabe-b7560c47cb92" in {
      val suuid = "c2001147-c8fa-4caf-aabe-b7560c47cb92"
      val scuuid = "5u4XcXOdzqDmt6iBkE7pbe"
      val uuid = UUID.fromString(suuid)
      val encoded = CompactUuid.encode(uuid)
      encoded should be(scuuid)
      val decoded = CompactUuid.decode(encoded)
      decoded should be(uuid)
    }
    "a57fb437-9b34-49d1-9e7b-e29a1dc74345" in {
      val suuid = "a57fb437-9b34-49d1-9e7b-e29a1dc74345"
      val scuuid = "52I5sX4VnJkgprtIa6B7uH"
      val uuid = UUID.fromString(suuid)
      val encoded = CompactUuid.encode(uuid)
      encoded should be(scuuid)
      val decoded = CompactUuid.decode(encoded)
      decoded should be(uuid)
    }
    "9c8aea35-572f-49d8-98a5-8a806b1a06e2" in {
      val suuid = "9c8aea35-572f-49d8-98a5-8a806b1a06e2"
      val scuuid = "4lOHoKGd2zQQNwoBb2kgh0"
      val uuid = UUID.fromString(suuid)
      val encoded = CompactUuid.encode(uuid)
      encoded should be(scuuid)
      val decoded = CompactUuid.decode(encoded)
      decoded should be(uuid)
    }
    "15294bb8-b23d-47b6-a1c5-90d2ec76b137" in {
      val suuid = "15294bb8-b23d-47b6-a1c5-90d2ec76b137"
      val scuuid = "dvi5RdfagsgCOKOYz06Eh"
      val uuid = UUID.fromString(suuid)
      val encoded = CompactUuid.encode(uuid)
      encoded should be(scuuid)
      val decoded = CompactUuid.decode(encoded)
      decoded should be(uuid)
    }
    "fd627ad3-5e28-4308-adb3-63294ae67345" in {
      val suuid = "fd627ad3-5e28-4308-adb3-63294ae67345"
      val scuuid = "7i83ySjUVfAioXuYEUtWvF"
      val uuid = UUID.fromString(suuid)
      val encoded = CompactUuid.encode(uuid)
      encoded should be(scuuid)
      val decoded = CompactUuid.decode(encoded)
      decoded should be(uuid)
    }
    "89a72bae-67cf-4c44-a938-905108838dfb" in {
      val suuid = "89a72bae-67cf-4c44-a938-905108838dfb"
      val scuuid = "4BkLRylilL4jMnGnfBbDXX"
      val uuid = UUID.fromString(suuid)
      val encoded = CompactUuid.encode(uuid)
      encoded should be(scuuid)
      val decoded = CompactUuid.decode(encoded)
      decoded should be(uuid)
    }
  }
}
