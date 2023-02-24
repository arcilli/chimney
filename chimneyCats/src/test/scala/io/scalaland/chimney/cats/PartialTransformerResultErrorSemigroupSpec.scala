package io.scalaland.chimney.cats

import _root_.cats.syntax.semigroup._
import io.scalaland.chimney.{PartialTransformer, partial}
import utest._

object PartialTransformerResultErrorSemigroupSpec extends TestSuite {

  val tests = Tests {

    test("Semigroup[partial.Result.Errors] should aggregate errors from 2 partial.Result.Errors") {

      val e1 = partial.Result.Errors.fromString("test1")
      val e2 = partial.Result.Errors.fromString("test2")

      (e1 |+| e2) ==> partial.Result.Errors.fromStrings("test1", "test2")
    }
  }
}
