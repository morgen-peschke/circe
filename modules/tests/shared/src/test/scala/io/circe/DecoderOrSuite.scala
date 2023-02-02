package io.circe

import cats.data.{NonEmptyChain, NonEmptyList}
import cats.syntax.all._
import cats.{Eq, Show}
import io.circe.DecodingFailure.Reason
import io.circe.syntax._
import io.circe.tests.CirceMunitSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Shrink}

class DecoderOrSuite extends CirceMunitSuite with DecodingFailureTestHelpers {
  import DecoderOrSuiteModels._
  private implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  private def expectedBoolean(value: Json, cursor: ACursor): DecodingFailure =
    DecodingFailure(Reason.WrongTypeExpectation("'true' or 'false'", value.asJson), cursor)

  private def expectedString(value: Json, cursor: ACursor): DecodingFailure =
    DecodingFailure(Reason.WrongTypeExpectation("string", value.asJson), cursor)

  private def expectedInt(cursor: ACursor): DecodingFailure = DecodingFailure("Int", cursor.history)

  private def expectedFixedString(value: String)(cursor: ACursor): DecodingFailure =
    DecodingFailure(s"""Expected "$value"""", cursor.history)

  private val expectedA = expectedFixedString("A")(_)
  private val expectedC = expectedFixedString("C")(_)
  private val expectedD = expectedFixedString("D")(_)

  private def aggregateFailure(cursor: ACursor)(df0: ACursor => DecodingFailure,
                                        dfN: ACursor => DecodingFailure*): DecodingFailure =
    DecodingFailure(Reason.AggregateDecodeFailure(NonEmptyList(df0, dfN.toList).map(_(cursor))), cursor)

  private val numStrings = Gen.stringOfN(5, Gen.numChar)
  private val fieldsForInvalidADT = Gen.zip(numStrings, arbitrary[List[Int]], arbitrary[Int], arbitrary[Boolean])

  property("decode preferring the left hand side") {
    val gen = Gen.oneOf(
      Gen.const("A".asJson -> UnwrappedADT.A),
      numStrings.map(s => s.asJson -> UnwrappedADT.S(s)),
      arbitrary[Boolean].map(b => b.asJson -> UnwrappedADT.B(b)),
      arbitrary[Int].map(i => i.asJson -> UnwrappedADT.I(i))
    )
    forAll(gen) {
      case (json, result) =>
        checkDecodeAccumulating[UnwrappedADT](json, result.valid) && checkDecode[UnwrappedADT](json, result.asRight)
    }
  }

  property("return combined failures, regardless of decoding mode") {
    forAll(fieldsForInvalidADT) { case (a, s, b, i) =>
      val json = Json.obj("f" := true, "adt" -> Json.obj("a" := a, "s" := s, "b" := b, "i" := i))
      val expected = aggregateFailure(json.hcursor.downField("adt"))(
        c => expectedA(c.downField("a")),
        c => expectedString(s.asJson, c.downField("s")),
        c => expectedBoolean(b.asJson, c.downField("b")),
        c => expectedInt(c.downField("i"))
      )

      checkDecodeAccumulating[WrappedSample](json, expected.invalidNel) &&
        checkDecode[WrappedSample](json, expected.asLeft)
    }
  }

  property("not squish errors at different levels (wrapped)") {
    forAll(arbitrary[Int], fieldsForInvalidADT){ case (f, (a, s, b, i)) =>
      val json = Json.obj("f" := f, "adt" -> Json.obj("a" := a, "s" := s, "b" := b, "i" := i))
      val failureInF = expectedBoolean(f.asJson, json.hcursor.downField("f"))
      val failureInADT = aggregateFailure(json.hcursor.downField("adt"))(
        c => expectedA(c.downField("a")),
        c => expectedString(s.asJson, c.downField("s")),
        c => expectedBoolean(b.asJson, c.downField("b")),
        c => expectedInt(c.downField("i"))
      )

      checkDecodeAccumulating[WrappedSample](json, NonEmptyList.of(failureInF, failureInADT).invalid) &&
        checkDecode[WrappedSample](json, failureInF.asLeft)
    }
  }

  property("not squish errors at different levels (unwrapped)") {
    forAll { (f: Int, adt: List[Int]) =>
      val json = Json.obj("f" := f, "adt" := adt)
      val failureInF = expectedBoolean(f.asJson, json.hcursor.downField("f"))
      val failureInADT = aggregateFailure(json.hcursor.downField("adt"))(
        expectedA,
        expectedString(adt.asJson, _),
        expectedBoolean(adt.asJson, _),
        expectedInt
      )

      checkDecodeAccumulating[UnwrappedSample](json, NonEmptyList.of(failureInF, failureInADT).invalid) &&
        checkDecode[UnwrappedSample](json, failureInF.asLeft)
    }
  }

  property("not squish errors at different levels (wrapped nested)") {
    forAll(numStrings, numStrings, fieldsForInvalidADT) { case (c, d, (a, s, b, i)) =>
      val json = Json.obj("c" := c, "d" := d, "n" -> Json.obj("a" := a, "s" := s, "b" := b, "i" := i))
      val expected = aggregateFailure(json.hcursor)(
        c => expectedC(c.downField("c")),
        c => expectedD(c.downField("d")),
        c => aggregateFailure(c.downField("n"))(
          sc => expectedA(sc.downField("a")),
          sc => expectedString(s.asJson, sc.downField("s")),
          sc => expectedBoolean(b.asJson, sc.downField("b")),
          sc => expectedInt(sc.downField("i"))
        )
      )

      checkDecodeAccumulating[WrappedNestedADT](json, expected.invalidNel) &&
        checkDecode[WrappedNestedADT](json, expected.asLeft)
    }
  }

  property("squish errors with a parent cursor") {
    forAll(numStrings, numStrings, fieldsForInvalidADT) { case (c, d, (a, s, b, i)) =>
      val json = Json.obj("c" := c, "d" := d, "a" := a, "s" := s, "b" := b, "i" := i)
      val expected = aggregateFailure(json.hcursor)(
        c => expectedC(c.downField("c")),
        c => expectedD(c.downField("d")),
        c => expectedA(c.downField("a")),
        c => expectedString(s.asJson, c.downField("s")),
        c => expectedBoolean(b.asJson, c.downField("b")),
        c => expectedInt(c.downField("i"))
      )

      checkDecodeAccumulating[ExtendingADT](json, expected.invalidNel) &&
        checkDecode[ExtendingADT](json, expected.asLeft)
    }
  }
}
object DecoderOrSuiteModels {
  private def decodeFixedString(value: String): Decoder[Unit] = {
    val errorMsg = s"""Expected "$value""""
    val error = errorMsg.asLeft
    Decoder[String].withErrorMessage(errorMsg).emap[Unit] {
      case `value` => ().asRight
      case _ => error
    }
  }
  private val decodeJustA: Decoder[Unit] = decodeFixedString("A")

  sealed trait UnwrappedADT

  object UnwrappedADT {
    case object A extends UnwrappedADT

    final case class S(s: String) extends UnwrappedADT

    final case class B(b: Boolean) extends UnwrappedADT

    final case class I(i: Int) extends UnwrappedADT

    implicit val aDecoder: Decoder[A.type] = decodeJustA.as(A)
    implicit val sDecoder: Decoder[S] = Decoder[String].map(S)
    implicit val bDecoder: Decoder[B] = Decoder.instance(_.as[Boolean].map(B))
    implicit val iDecoder: Decoder[I] = Decoder.instance(_.as[Int].map(I))

    implicit val decoder: Decoder[UnwrappedADT] = NonEmptyChain.of[Decoder[UnwrappedADT]](
      aDecoder.widen,
      sDecoder.widen,
      bDecoder.widen,
      iDecoder.widen
    ).reduceLeft(_ or _)

    implicit val show: Show[UnwrappedADT] = Show.fromToString
    implicit val eq: Eq[UnwrappedADT] = Eq.fromUniversalEquals
  }

  sealed trait WrappedADT

  object WrappedADT {
    case object A extends WrappedADT

    final case class S(s: String) extends WrappedADT

    final case class B(b: Boolean) extends WrappedADT

    final case class I(i: Int) extends WrappedADT

    implicit val aDecoder: Decoder[A.type] = decodeJustA.at("a").as(A)
    implicit val sDecoder: Decoder[S] = Decoder[String].at("s").map(S)
    implicit val bDecoder: Decoder[B] = Decoder.instance(_.downField("b").as[Boolean].map(B))
    implicit val iDecoder: Decoder[I] = Decoder.instance(_.downField("i").as[Int].map(I))

    implicit val decoder: Decoder[WrappedADT] = NonEmptyChain.of[Decoder[WrappedADT]](
      aDecoder.widen,
      sDecoder.widen,
      bDecoder.widen,
      iDecoder.widen
    ).reduceLeft(_ or _)

    implicit val show: Show[WrappedADT] = Show.fromToString
    implicit val eq: Eq[WrappedADT] = Eq.fromUniversalEquals
  }

  final case class UnwrappedSample(f: Boolean, adt: UnwrappedADT)

  object UnwrappedSample {
    implicit val decoder: Decoder[UnwrappedSample] = Decoder.instanceAccumulating { cursor =>
      (cursor.downField("f").asAccumulating[Boolean], cursor.downField("adt").asAccumulating[UnwrappedADT])
        .mapN(UnwrappedSample(_, _))
    }

    implicit val show: Show[UnwrappedSample] = Show.fromToString
    implicit val eq: Eq[UnwrappedSample] = Eq.fromUniversalEquals
  }

  final case class WrappedSample(f: Boolean, adt: WrappedADT)

  object WrappedSample {
    implicit val decoder: Decoder[WrappedSample] = Decoder.instanceAccumulating { cursor =>
      (cursor.downField("f").asAccumulating[Boolean], cursor.downField("adt").asAccumulating[WrappedADT])
        .mapN(WrappedSample(_, _))
    }

    implicit val show: Show[WrappedSample] = Show.fromToString
    implicit val eq: Eq[WrappedSample] = Eq.fromUniversalEquals
  }

  sealed trait WrappedNestedADT
  object WrappedNestedADT {
    case object C extends WrappedNestedADT
    case object D extends WrappedNestedADT
    final case class N(wrappedADT: WrappedADT) extends WrappedNestedADT

    implicit val cDecoder: Decoder[C.type] = decodeFixedString("C").at("c").as(C)
    implicit val dDecoder: Decoder[D.type] = decodeFixedString("D").at("d").as(D)
    implicit val nDecoder: Decoder[N] = Decoder[WrappedADT].at("n").map(N)

    implicit val decoder: Decoder[WrappedNestedADT] = NonEmptyChain.of[Decoder[WrappedNestedADT]](
      cDecoder.widen,
      dDecoder.widen,
      nDecoder.widen
    ).reduceLeft(_ or _)

    implicit val show: Show[WrappedNestedADT] = Show.fromToString
    implicit val eq: Eq[WrappedNestedADT] = Eq.fromUniversalEquals
  }

  sealed trait ExtendingADT

  object ExtendingADT {
    case object C extends ExtendingADT
    case object D extends ExtendingADT
    final case class N(wrappedADT: WrappedADT) extends ExtendingADT

    implicit val cDecoder: Decoder[C.type] = decodeFixedString("C").at("c").as(C)
    implicit val dDecoder: Decoder[D.type] = decodeFixedString("D").at("d").as(D)
    implicit val nDecoder: Decoder[N] = Decoder[WrappedADT].map(N)

    implicit val decoder: Decoder[ExtendingADT] = NonEmptyChain.of[Decoder[ExtendingADT]](
      cDecoder.widen,
      dDecoder.widen,
      nDecoder.widen
    ).reduceLeft(_ or _)

    implicit val show: Show[ExtendingADT] = Show.fromToString
    implicit val eq: Eq[ExtendingADT] = Eq.fromUniversalEquals
  }
}