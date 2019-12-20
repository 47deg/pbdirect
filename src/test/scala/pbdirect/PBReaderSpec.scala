package pbdirect

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import enumeratum.values._

class PBReaderSpec extends AnyWordSpecLike with Matchers {

  "PBReader" should {
    "read a Boolean from Protobuf" in {
      case class BooleanMessage(@pbIndex(1) value: Option[Boolean])
      val bytes = Array[Byte](8, 1)
      bytes.pbTo[BooleanMessage] shouldBe BooleanMessage(Some(true))
    }
    "read a Byte from Protobuf" in {
      case class ByteMessage(@pbIndex(1) value: Option[Byte])
      val bytes = Array[Byte](8, 32)
      bytes.pbTo[ByteMessage] shouldBe ByteMessage(Some(32))
    }
    "read a Short from Protobuf" in {
      case class ShortMessage(@pbIndex(1) value: Option[Short])
      val bytes = Array[Byte](8, -1, 63)
      bytes.pbTo[ShortMessage] shouldBe ShortMessage(Some(8191))
    }
    "read an Int from Protobuf" in {
      case class IntMessage(@pbIndex(1) value: Option[Int])
      val bytes = Array[Byte](8, 5)
      bytes.pbTo[IntMessage] shouldBe IntMessage(Some(5))
    }
    "read a Long from Protobuf" in {
      case class LongMessage(@pbIndex(1) value: Option[Long])
      val bytes = Array[Byte](8, -128, -128, -128, -128, 8)
      bytes.pbTo[LongMessage] shouldBe LongMessage(Some(Int.MaxValue.toLong + 1))
    }
    "read a Float from Protobuf" in {
      case class FloatMessage(@pbIndex(1) value: Option[Float])
      val bytes = Array[Byte](13, -51, -52, 76, 62)
      bytes.pbTo[FloatMessage] shouldBe FloatMessage(Some(0.2F))
    }
    "read a Double from Protobuf" in {
      case class DoubleMessage(@pbIndex(1) value: Option[Double])
      val bytes = Array[Byte](9, -107, 100, 121, -31, 127, -3, -75, 61)
      bytes.pbTo[DoubleMessage] shouldBe DoubleMessage(Some(0.00000000002D))
    }
    "read a String from Protobuf" in {
      case class StringMessage(@pbIndex(1) value: Option[String])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[StringMessage] shouldBe StringMessage(Some("Hello"))
    }
    "read bytes from Protobuf" in {
      case class BytesMessage(@pbIndex(1) value: Option[Array[Byte]])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[BytesMessage].value.get shouldBe Array[Byte](72, 101, 108, 108, 111)
    }
    "read an enumeration from Protobuf" in {
      case object Grade extends Enumeration {
        val GradeA, GradeB = Value
      }
      val bytesA = Array[Byte](8, 0)
      val bytesB = Array[Byte](8, 1)
      case class GradeMessage(@pbIndex(1) value: Option[Grade.Value])
      bytesA.pbTo[GradeMessage] shouldBe GradeMessage(Some(Grade.GradeA))
      bytesB.pbTo[GradeMessage] shouldBe GradeMessage(Some(Grade.GradeB))
    }
    "read an enum from Protobuf" in {
      sealed trait Grade extends Pos
      case object GradeA extends Grade with Pos._0
      case object GradeB extends Grade with Pos._1
      case class GradeMessage(@pbIndex(1) value: Option[Grade])
      val bytesA = Array[Byte](8, 0)
      val bytesB = Array[Byte](8, 1)
      bytesA.pbTo[GradeMessage] shouldBe GradeMessage(Some(GradeA))
      bytesB.pbTo[GradeMessage] shouldBe GradeMessage(Some(GradeB))
    }
    "read an enumeratum IntEnumEntry from Protobuf" in {
      case class QualityMessage(@pbIndex(1) quality: Quality)
      val bytes = Array[Byte](8, 3)
      bytes.pbTo[QualityMessage] shouldBe QualityMessage(Quality.OK)
    }
    "read a required field from Protobuf" in {
      case class RequiredMessage(@pbIndex(1) value: Int)
      val bytes = Array[Byte](8, 5)
      bytes.pbTo[RequiredMessage] shouldBe RequiredMessage(5)
    }
    "read an empty message from Protobuf" in {
      case class EmptyMessage()
      val bytes = Array[Byte]()
      bytes.pbTo[EmptyMessage] shouldBe EmptyMessage()
    }
    "read a multi-field message from Protobuf" in {
      case class MultiMessage(@pbIndex(1) text: Option[String], @pbIndex(2) number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 16, 3)
      bytes.pbTo[MultiMessage] shouldBe MultiMessage(Some("Hello"), Some(3))
    }
    "read a multi-field message without pbIndex annotations from Protobuf" in {
      case class MultiMessage(text: Option[String], number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 16, 3)
      bytes.pbTo[MultiMessage] shouldBe MultiMessage(Some("Hello"), Some(3))
    }
    "read a message with missing field from Protobuf" in {
      case class MissingMessage(@pbIndex(1) text: Option[String], @pbIndex(2) number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[MissingMessage] shouldBe MissingMessage(Some("Hello"), None)
    }
    "read a message with repeated field from Protobuf" in {
      case class RepeatedMessage(@pbIndex(1) values: List[Int])
      val bytes = Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
      bytes.pbTo[RepeatedMessage] shouldBe RepeatedMessage(1 :: 2 :: 3 :: 4 :: Nil)
    }
    "read a message with Seq from Protobuf" in {
      case class RepeatedMessage(@pbIndex(1) values: Seq[Int])
      val bytes = Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
      bytes.pbTo[RepeatedMessage] shouldBe RepeatedMessage(Seq(1, 2, 3, 4))
    }
    "read a Map from Protobuf" in {
      case class MapMessage(@pbIndex(1) values: Map[Int, String])
      val bytes = Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
      bytes.pbTo[MapMessage] shouldBe MapMessage(Map(1 -> "one", 2 -> "two"))
    }
    "read a scala.collection.Map from Protobuf" in {
      case class MapMessage(@pbIndex(1) values: collection.Map[Int, String])
      val bytes = Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
      bytes.pbTo[MapMessage] shouldBe MapMessage(collection.Map(1 -> "one", 2 -> "two"))
    }
    "read a nested message from Protobuf" in {
      case class InnerMessage(@pbIndex(1) value: Option[Int])
      case class OuterMessage(
          @pbIndex(1) text: Option[String],
          @pbIndex(2) inner: Option[InnerMessage])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 18, 2, 8, 11)
      bytes.pbTo[OuterMessage] shouldBe OuterMessage(Some("Hello"), Some(InnerMessage(Some(11))))
    }
    // TODO decide what to do with this
    "read a sealed trait from Protobuf" in {
      sealed trait Message
      case class IntMessage(@pbIndex(1) value: Option[Int])       extends Message
      case class StringMessage(@pbIndex(1) value: Option[String]) extends Message
      val intBytes    = Array[Byte](8, 5)
      val stringBytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      intBytes.pbTo[Message] shouldBe IntMessage(Some(5))
      stringBytes.pbTo[Message] shouldBe StringMessage(Some("Hello"))
    }
    "read a message with repeated nested message from Protobuf" in {
      case class Metric(
          @pbIndex(1) name: String,
          @pbIndex(2) service: String,
          @pbIndex(3) node: String,
          @pbIndex(4) value: Float,
          @pbIndex(5) count: Int)
      case class Metrics(@pbIndex(1) metrics: List[Metric])
      val message = Metrics(
        Metric("metric", "microservices", "node", 12F, 12345) :: Nil
      )
      val bytes = Array[Byte](10, 37, 10, 6, 109, 101, 116, 114, 105, 99, 18, 13, 109, 105, 99, 114,
        111, 115, 101, 114, 118, 105, 99, 101, 115, 26, 4, 110, 111, 100, 101, 37, 0, 0, 64, 65, 40,
        -71, 96)
      bytes.pbTo[Metrics] shouldBe message
    }
    "derive new instance using map" in {
      import java.time.Instant
      import cats.syntax.functor._
      implicit val instantReader: PBScalarValueReader[Instant] =
        PBScalarValueReader[Long].map(Instant.ofEpochMilli)
      case class Message(@pbIndex(1) instant: Instant)
      val instant = Instant.ofEpochMilli(1499411227777L)
      Array[Byte](8, -127, -55, -2, -34, -47, 43).pbTo[Message] shouldBe Message(instant)
    }
    "read a message with fields out-of-order from Protobuf" in {
      case class MultiMessage(@pbIndex(1) text: Option[String], @pbIndex(2) number: Option[Int])
      val bytes = Array[Byte](16, 3, 10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[MultiMessage] shouldBe MultiMessage(Some("Hello"), Some(3))
    }
    "read a message with non-sequential field indices from Protobuf" in {
      case class MultiMessage(@pbIndex(1) text: Option[String], @pbIndex(3) number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 24, 3)
      bytes.pbTo[MultiMessage] shouldBe MultiMessage(Some("Hello"), Some(3))
    }
  }
}

// For some reason it fails to resolve the implicit PBReader if the enum is defined inside the test class
sealed abstract class Quality(val value: Int) extends IntEnumEntry

object Quality extends IntEnum[Quality] {
  case object Good extends Quality(0)
  case object OK   extends Quality(3)
  case object Bad  extends Quality(5)

  val values = findValues
}
