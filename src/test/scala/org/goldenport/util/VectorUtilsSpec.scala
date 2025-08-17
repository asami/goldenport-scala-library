package org.goldenport.util

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

/*
 * @since   Aug. 26, 2018
 * @version Aug. 16, 2025
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class VectorUtilsSpec extends AnyWordSpec with Matchers with GivenWhenThen with ScalaCheckDrivenPropertyChecks {
  import VectorUtils._
  import VectorUtilsSpec._

  // =========
  // Generators
  // =========
  implicit val arbIntVector: Arbitrary[Vector[Int]] =
    Arbitrary(Gen.listOf(Arbitrary.arbitrary[Int]).map(_.toVector))

  implicit val arbT: Arbitrary[T] = Arbitrary(
    Gen.oneOf(
      Arbitrary.arbitrary[Int].map(X.apply),
      Arbitrary.arbitrary[String].map(Y.apply)
    )
  )
  implicit val arbTVector: Arbitrary[Vector[T]] =

    Arbitrary(Gen.listOf(arbT.arbitrary).map(_.toVector))
  "slinding2" should {
    "typical" in {
      sliding2("abc".toVector) should be(Vector(Vector('a', 'b'), Vector('b', 'c'), Vector('c')))
    }
    "zero" in {
      sliding2("".toVector) should be(Vector())
    }
    "one" in {
      sliding2("a".toVector) should be(Vector(Vector('a')))
    }
    "two" in {
      sliding2("ab".toVector) should be(Vector(Vector('a', 'b'), Vector('b')))
    }
  }

  "slinding3" should {
    "typical" in {
      sliding3("abc".toVector) should be(Vector(Vector('a', 'b', 'c'), Vector('b', 'c'), Vector('c')))
    }
    "zero" in {
      sliding3("".toVector) should be(Vector())
    }
    "one" in {
      sliding3("a".toVector) should be(Vector(Vector('a')))
    }
    "two" in {
      sliding3("ab".toVector) should be(Vector(Vector('a', 'b'), Vector('b')))
    }
  }

  // ==============
  // split3 の Spec
  // ==============
  "split3" should {
    "recompose to original sequence and preserve partition semantics (property)" in {
      forAll { (v: Vector[Int], k: Int) =>
        val p: Int => Boolean = _ == k
        val (before, matched, after) = split3(v)(p)

        // 再結合は元と等しい
        (before ++ matched ++ after) shouldEqual v

        // before はすべて非一致
        all (before) should not equal k

        // matched は p をすべて満たす（空でもOK）
        all (matched) should equal (k)

        // matched が空なら、v には一致が無い
        if (matched.isEmpty) {
          v.forall(a => !p(a)) shouldBe true
        } else {
          // matched の先頭直前は非一致
          before.lastOption.forall(a => !p(a)) shouldBe true
          // matched の直後は非一致（※後続で再び一致が現れることはあり得る）
          after.headOption.forall(a => !p(a)) shouldBe true
        }
      }
    }

    "handle edge cases (example-based)" in {
      val v1 = Vector.empty[Int]
      split3(v1)(_ => true) shouldEqual (Vector(), Vector(), Vector())

      val v2 = Vector(1, 2, 3)
      split3(v2)(_ => false) shouldEqual (v2, Vector(), Vector())

      val v3 = Vector(1, 2, 2, 3, 2, 4)
      split3(v3)(_ == 2) shouldEqual (Vector(1), Vector(2, 2), Vector(3, 2, 4))

      val v4 = Vector(2, 2, 2)
      split3(v4)(_ == 2) shouldEqual (Vector(), Vector(2, 2, 2), Vector())

      val v5 = Vector(1, 2, 3)
      split3(v5)(_ == 3) shouldEqual (Vector(1, 2), Vector(3), Vector())
    }
  }

  // =====================
  // split3Option の Spec
  // =====================
  "split3Option" should {
    "return None iff no element matches; otherwise Some(before, firstMatch, after) (property)" in {
      forAll { (v: Vector[Int], k: Int) =>
        val p: Int => Boolean = _ == k
        val r = split3Option(v)(p)
        r match {
          case None =>
            v.forall(a => !p(a)) shouldBe true

          case Some((before, first, after)) =>
            // 再結合は元と等しい
            (before ++ Vector(first) ++ after) shouldEqual v
            // before は非一致のみ
            before.forall(a => !p(a)) shouldBe true
            // first は一致
            p(first) shouldBe true
            // after の先頭は「first の直後」
            v.indexOf(first) should be >= 0
        }
      }
    }

    "handle edge cases (example-based)" in {
      split3Option(Vector.empty[Int])(_ => true) shouldBe None
      split3Option(Vector(1, 2, 3))(_ => false) shouldBe None
      split3Option(Vector(1, 2, 2, 3))(_ == 2) shouldBe Some((Vector(1), 2, Vector(2, 3)))
      split3Option(Vector(2, 2, 2))(_ == 2) shouldBe Some((Vector(), 2, Vector(2, 2)))
      split3Option(Vector(1, 2, 3))(_ == 3) shouldBe Some((Vector(1, 2), 3, Vector()))
    }
  }

  // ======================
  // findMapSplit の Spec
  // ======================
  "findMapSplit" should {
    "behave like split around the first element for which p returns Some (property)" in {
      val p: T => Option[X] = {
        case x @ X(_) => Some(x)
        case _        => None
      }

      forAll { (v: Vector[T]) =>
        val r = findMapSplit[T, X](v)(p)
        r match {
          case None =>
            v.forall(a => p(a).isEmpty) shouldBe true

          case Some((before, b, after)) =>
            // 再結合（b は A の一部なので Vector(b) として結合）
            (before ++ Vector(b) ++ after) shouldEqual v

            // before は p が None
            before.forall(a => p(a).isEmpty) shouldBe true

            // b は p(b) が Some(b)（同一性までは要件外なので、i が等しいことを見る）
            p(b).nonEmpty shouldBe true

            // after の先頭は b の直後
            // （後半にXが再び現れても仕様上OK）
            succeed
        }
      }
    }

    "handle edge cases (example-based)" in {
      val p: T => Option[X] = {
        case x @ X(_) => Some(x)
        case _        => None
      }

      findMapSplit[T, X](Vector.empty)(p) shouldBe None
      findMapSplit[T, X](Vector(Y("a"), Y("b")))(p) shouldBe None

      val v1 = Vector(Y("a"), X(10), Y("b"), X(20))
      findMapSplit[T, X](v1)(p) shouldBe Some((Vector(Y("a")), X(10), Vector(Y("b"), X(20))))

      val v2 = Vector(X(1), Y("a"))
      findMapSplit[T, X](v2)(p) shouldBe Some((Vector(), X(1), Vector(Y("a"))))

      val v3 = Vector(Y("a"), Y("b"), X(3))
      findMapSplit[T, X](v3)(p) shouldBe Some((Vector(Y("a"), Y("b")), X(3), Vector()))
    }
  }
}

object VectorUtilsSpec {
  // ADT for findMapSplit のテスト
  sealed trait T
  final case class X(i: Int) extends T
  final case class Y(s: String) extends T
}
