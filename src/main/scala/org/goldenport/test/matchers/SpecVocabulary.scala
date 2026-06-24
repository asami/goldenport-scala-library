package org.goldenport.test.matchers

import java.nio.file.{Files, Path}
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.matchers.should.Matchers

/*
 * @since   Jun. 20, 2026
 * @version Jun. 25, 2026
 * @author  ASAMI, Tomoharu
 */
trait SpecVocabulary extends Matchers {
  protected final def be_regular_file: Matcher[Path] = Matcher { path =>
    MatchResult(
      Files.isRegularFile(path),
      s"${path} was not a regular file",
      s"${path} was a regular file"
    )
  }

  protected final def exist_path: Matcher[Path] = Matcher { path =>
    MatchResult(
      Files.exists(path),
      s"${path} did not exist",
      s"${path} existed"
    )
  }

  protected final def contain_where[A](predicate: A => Boolean): Matcher[Iterable[A]] = Matcher { values =>
    MatchResult(
      values.exists(predicate),
      s"${values} did not contain an element satisfying the predicate",
      s"${values} contained an element satisfying the predicate"
    )
  }

  protected final def not_contain_where[A](predicate: A => Boolean): Matcher[Iterable[A]] = Matcher { values =>
    MatchResult(
      !values.exists(predicate),
      s"${values} contained an element satisfying the predicate",
      s"${values} did not contain an element satisfying the predicate"
    )
  }
}
