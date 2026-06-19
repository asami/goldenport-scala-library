package org.goldenport.test.matchers

import java.nio.file.{Files, Path}
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.matchers.should.Matchers

/*
 * @since   Jun. 20, 2026
 * @version Jun. 20, 2026
 * @author  ASAMI, Tomoharu
 */
trait SpecVocabulary extends Matchers {
  def beRegularFile: Matcher[Path] = Matcher { path =>
    MatchResult(
      Files.isRegularFile(path),
      s"${path} was not a regular file",
      s"${path} was a regular file"
    )
  }

  def existPath: Matcher[Path] = Matcher { path =>
    MatchResult(
      Files.exists(path),
      s"${path} did not exist",
      s"${path} existed"
    )
  }

  def containWhere[A](predicate: A => Boolean): Matcher[Iterable[A]] = Matcher { values =>
    MatchResult(
      values.exists(predicate),
      s"${values} did not contain an element satisfying the predicate",
      s"${values} contained an element satisfying the predicate"
    )
  }

  def notContainWhere[A](predicate: A => Boolean): Matcher[Iterable[A]] = Matcher { values =>
    MatchResult(
      !values.exists(predicate),
      s"${values} contained an element satisfying the predicate",
      s"${values} did not contain an element satisfying the predicate"
    )
  }
}
