package zio.schema.codec

import java.time.{ ZoneId, ZoneOffset }

import scala.collection.immutable.ListMap

import zio.Console._
import zio._
import zio.json.JsonDecoder.JsonError
import zio.json.{ DeriveJsonEncoder, JsonEncoder }
import zio.schema.CaseSet._
import zio.schema._
import zio.schema.annotation._
import zio.schema.codec.DecodeError.ReadError
import zio.schema.codec.JsonCodec.JsonEncoder.charSequenceToByteChunk
import zio.schema.meta.MetaSchema
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object JsonCodecSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, Any] =
    suite("JsonCodec Spec")(
      encoderSuite,
    ) @@ timeout(90.seconds)

  private val encoderSuite = suite("encoding")(
    suite("primitive")(
      test("unit") {
        assertEncodesJson(Schema[Unit], (), "{}")
      }
    ),
    suite("Map")(
      test("of complex keys and values") {
        assertEncodes(
          Schema.map[Key, Value],
          Map(Key("a", 0) -> Value(0, true), Key("b", 1) -> Value(1, false)),
          charSequenceToByteChunk(
            """[[{"name":"a","index":0},{"first":0,"second":true}],[{"name":"b","index":1},{"first":1,"second":false}]]"""
          )
        )
      },
      test("of simple keys and values") {
        assertEncodes(
          Schema.map[Int, Value],
          Map(0 -> Value(0, true), 1 -> Value(1, false)),
          charSequenceToByteChunk(
            """{"0":{"first":0,"second":true},"1":{"first":1,"second":false}}"""
          )
        )
      }
    )
  )
  private def assertEncodes[A](schema: Schema[A], value: A, chunk: Chunk[Byte], print: Boolean = false) = {
    val stream = ZStream
      .succeed(value)
      .via(JsonCodec.schemaBasedBinaryCodec(schema).streamEncoder)
      .runCollect
      .tap { chunk =>
        printLine(s"${new String(chunk.toArray)}").when(print).ignore
      }
    assertZIO(stream)(equalTo(chunk))
  }

  private def assertEncodesJson[A](schema: Schema[A], value: A, json: String) = {
    val stream = ZStream
      .succeed(value)
      .via(JsonCodec.schemaBasedBinaryCodec[A](schema).streamEncoder)
      .runCollect
      .map(chunk => new String(chunk.toArray))
    assertZIO(stream)(equalTo(json))
  }
}

case class Key(name: String, index: Int)

object Key {
  implicit lazy val schema: Schema[Key] = DeriveSchema.gen[Key]
}
case class Value(first: Int, second: Boolean)

object Value {
  implicit lazy val schema: Schema[Value] = DeriveSchema.gen[Value]
}
