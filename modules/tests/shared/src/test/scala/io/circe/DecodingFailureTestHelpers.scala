package io.circe

import cats.{Eq, Show}
import cats.syntax.all._
import org.scalacheck.{Prop, Shrink}

trait DecodingFailureTestHelpers {
  implicit val useVerboseShowForDecodingFailures: Show[DecodingFailure] = DecodingFailure.showExpandedDecodingFailure

  // Works around issues supplying a custom Show to Scalacheck
  def checkDecode[A: Show : Eq : Decoder](json: Json, expected: Decoder.Result[A]): Prop = {
    val actual = json.as[A]
    (if (actual === expected) Prop.proved else Prop.falsified) :|
      show"""|Decode did not produce the expected value
             |Expected: $expected
             |Actual  : $actual
             |Raw JSON: ${json.printWith(Printer.spaces2)}""".stripMargin
  }

  def checkDecodeAccumulating[A: Show : Eq : Decoder](json: Json,
                                                              expected: Decoder.AccumulatingResult[A]): Prop = {
    val actual = json.asAccumulating[A]
    (if (actual === expected) Prop.proved else Prop.falsified) :|
      show"""|DecodeAccumulating did not produce the expected value
             |Expected: $expected
             |Actual  : $actual
             |Raw JSON: ${json.printWith(Printer.spaces2)}""".stripMargin
  }
}
