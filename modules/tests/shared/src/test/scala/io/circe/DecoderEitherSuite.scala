package io.circe

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.{Eq, Show}
import io.circe.DecodingFailure.Reason
import io.circe.syntax._
import io.circe.tests.CirceMunitSuite
import org.scalacheck.{Prop, Shrink}
import org.scalacheck.Prop._

class DecoderEitherSuite extends CirceMunitSuite with DecodingFailureTestHelpers {
  import DecoderEitherSuiteModels._
  private implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  def expectedBoolean(value: Json, cursor: ACursor): DecodingFailure =
    DecodingFailure(Reason.WrongTypeExpectation("'true' or 'false'", value.asJson), cursor)

  def expectedInt(cursor: ACursor): DecodingFailure = DecodingFailure("Int", cursor.history)

  def aggregateFailure(cursor: ACursor)(df0: DecodingFailure, dfN: DecodingFailure*): DecodingFailure =
    DecodingFailure(Reason.AggregateDecodeFailure(NonEmptyList(df0, dfN.toList)), cursor)

  // Works around issues supplying a custom Show to Scalacheck
  def verify[A: Show: Eq](actual: A, expected: A): Prop =
    (if (actual === expected) Prop.proved else Prop.falsified) :|
      show"""|Actual value was not expected value
             |Expected: $expected
             |Actual  : $actual""".stripMargin

  property("decode preferring the left result") {
    forAll { (a: Int, f: Boolean, s: Int) =>
      val json = Json.obj("a" := a, "tOrS" -> Json.obj(
        "toggle" := f,
        "slider" := s
      ))
      checkDecodeAccumulating[WrappedSample](json, WrappedSample(a, Toggle(f).asLeft).validNel) &&
        checkDecode[WrappedSample](json, WrappedSample(a, Toggle(f).asLeft).asRight)
    }
  }

  property("decode the right result when the left decoder fails (missing field)") {
    forAll { (a: Int, s: Int) =>
      val json = Json.obj("a" := a, "tOrS" -> Json.obj("slider" := s))
      checkDecodeAccumulating[WrappedSample](json, WrappedSample(a, Slider(s).asRight).validNel) &&
        checkDecode[WrappedSample](json, WrappedSample(a, Slider(s).asRight).asRight)
    }
  }

  property("decode the right result when the left decoder fails (wrong type)") {
    forAll { (a: Int, f: Int, s: Int) =>
      val json = Json.obj("a" := a, "tOrS" -> Json.obj(
        "toggle" := f,
        "slider" := s
      ))
      checkDecodeAccumulating[WrappedSample](json, WrappedSample(a, Slider(s).asRight).validNel) &&
        checkDecode[WrappedSample](json, WrappedSample(a, Slider(s).asRight).asRight)
    }
  }

  property("return combined failures, regardless of decoding mode") {
    forAll { (a: Int, f: Int, s: Boolean) =>
      val json = Json.obj("a" := a, "tOrS" -> Json.obj(
        "toggle" := f,
        "slider" := s
      ))

      val expected = aggregateFailure(json.hcursor.downField("tOrS"))(
        expectedBoolean(f.asJson, json.hcursor.downField("tOrS").downField("toggle")),
        expectedInt(json.hcursor.downField("tOrS").downField("slider"))
      )

      checkDecodeAccumulating[WrappedSample](json, expected.invalidNel) &&
        checkDecode[WrappedSample](json, expected.asLeft)
    }
  }

  property("not squish errors (wrapped)") {
    forAll { (a: Boolean, f: Int, s: Boolean) =>
      val json = Json.obj("a" := a, "tOrS" -> Json.obj(
        "toggle" := f,
        "slider" := s
      ))

      val aFailure = expectedInt(json.hcursor.downField("a"))
      val tOrSFailure = aggregateFailure(json.hcursor.downField("tOrS"))(
        expectedBoolean(f.asJson, json.hcursor.downField("tOrS").downField("toggle")),
        expectedInt(json.hcursor.downField("tOrS").downField("slider"))
      )

      checkDecodeAccumulating[WrappedSample](json, NonEmptyList.of(aFailure, tOrSFailure).invalid) &&
        checkDecode[WrappedSample](json, aFailure.asLeft)
    }
  }

  property("not squish errors (unwrapped)") {
    forAll { (a: Boolean, b: Boolean) =>
      val json = Json.obj("a" := a, "b" := List(b))

      val aFailure = expectedInt(json.hcursor.downField("a"))
      val tOrSFailure = aggregateFailure(json.hcursor.downField("b"))(
        expectedBoolean(List(b).asJson, json.hcursor.downField("b")),
        expectedInt(json.hcursor.downField("b"))
      )

      checkDecodeAccumulating[UnwrappedSample](json, NonEmptyList.of(aFailure, tOrSFailure).invalid) &&
        checkDecode[UnwrappedSample](json, aFailure.asLeft)
    }
  }
}
object DecoderEitherSuiteModels {
  final case class UnwrappedSample(a: Int, b: Either[Boolean, Int])

  object UnwrappedSample {
    implicit val eitherBoolOrIntDecoder: Decoder[Either[Boolean, Int]] =
      Decoder.decodeBoolean.either(Decoder.decodeInt)

    implicit val decoder: Decoder[UnwrappedSample] = Decoder.instanceAccumulating { cursor =>
      (cursor.downField("a").asAccumulating[Int], cursor.downField("b").asAccumulating[Either[Boolean, Int]])
        .mapN(UnwrappedSample(_, _))
    }

    implicit val show: Show[UnwrappedSample] = Show.fromToString
    implicit val eq: Eq[UnwrappedSample] = Eq.fromUniversalEquals
  }

  final case class Toggle(toggle: Boolean)

  final case class Slider(slider: Int)

  final case class WrappedSample(a: Int, tOrS: Either[Toggle, Slider])

  object WrappedSample {
    implicit val eitherToggleOrSliderDecoder: Decoder[Either[Toggle, Slider]] =
      Decoder.decodeBoolean.at("toggle").map(Toggle)
        .either(Decoder.decodeInt.at("slider").map(Slider))

    implicit val decoder: Decoder[WrappedSample] = Decoder.instanceAccumulating { cursor =>
      (cursor.downField("a").asAccumulating[Int], cursor.downField("tOrS").asAccumulating[Either[Toggle, Slider]])
        .mapN(WrappedSample(_, _))
    }

    implicit val show: Show[WrappedSample] = Show.fromToString
    implicit val eq: Eq[WrappedSample] = Eq.fromUniversalEquals
  }
}