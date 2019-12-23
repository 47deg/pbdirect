package pbdirect

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import shapeless._
import shapeless.syntax.inject._

class PBMessageWriterSpec extends AnyWordSpecLike with Matchers {
  "PBMessageWriter" should {
    "write an empty message to Protobuf" in {
      case class EmptyMessage()
      val message = EmptyMessage()
      message.toPB shouldBe Array[Byte]()
    }
    "write a message with a required field to Protobuf" in {
      case class RequiredMessage(@pbIndex(1) value: Int)
      val message = RequiredMessage(5)
      message.toPB shouldBe Array[Byte](8, 5)
    }
    "write a message with an optional field to Protobuf" in {
      case class OptionalMessage(@pbIndex(1) value: Option[Int])
      val message = OptionalMessage(Some(5))
      message.toPB shouldBe Array[Byte](8, 5)
    }
    "write a message with an empty optional field to Protobuf" in {
      case class OptionalMessage(@pbIndex(1) value: Option[Int])
      val message = OptionalMessage(None)
      message.toPB shouldBe Array[Byte]()
    }
    "write a message with missing field to Protobuf" in {
      case class MissingMessage(@pbIndex(1) text: Option[String], @pbIndex(2) number: Option[Int])
      val message = MissingMessage(Some("Hello"), None)
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111)
    }
    "write a multi-field message to Protobuf" in {
      case class MultiMessage(@pbIndex(1) text: Option[String], @pbIndex(2) number: Option[Int])
      val message = MultiMessage(Some("Hello"), Some(3))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111, 16, 3)
    }
    "write a multi-field message without pbIndex annotations to Protobuf" in {
      case class MultiMessage(text: Option[String], number: Option[Int])
      val message = MultiMessage(Some("Hello"), Some(3))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111, 16, 3)
    }
    "write a message with repeated field to Protobuf" in {
      case class RepeatedMessage(@pbIndex(1) values: List[Int])
      val message = RepeatedMessage(1 :: 2 :: 3 :: 4 :: Nil)
      message.toPB shouldBe Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
    }
    "write a message with an embedded message to Protobuf" in {
      case class InnerMessage(@pbIndex(1) value: Option[Int])
      case class OuterMessage(
          @pbIndex(1) text: Option[String],
          @pbIndex(2) inner: Option[InnerMessage])
      val message = OuterMessage(Some("Hello"), Some(InnerMessage(Some(11))))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111, 18, 2, 8, 11)
    }
    "write a nested message with the inner message's fields all missing to Protobuf" in {
      case class InnerMessage(@pbIndex(1) value: Option[Int])
      case class OuterMessage(
          @pbIndex(1) text: Option[String],
          @pbIndex(2) inner: Option[InnerMessage])
      val message = OuterMessage(Some("Hello"), Some(InnerMessage(None)))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111, 18, 0)
    }
    "write a nested message with the whole inner message missing to Protobuf" in {
      case class InnerMessage(@pbIndex(1) value: Option[Int])
      case class OuterMessage(
          @pbIndex(1) text: Option[String],
          @pbIndex(2) inner: Option[InnerMessage])
      val message = OuterMessage(Some("Hello"), None)
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111)
    }
    "write a message with repeated nested message in Protobuf" in {
      case class Metric(
          @pbIndex(1) metric: String,
          @pbIndex(2) microservice: String,
          @pbIndex(3) node: String,
          @pbIndex(4) value: Float,
          @pbIndex(5) count: Int)
      case class Metrics(@pbIndex(1) metrics: List[Metric])
      val message = Metrics(
        Metric("metric", "microservices", "node", 12F, 12345) :: Nil
      )
      message.toPB shouldBe Array[Byte](10, 37, 10, 6, 109, 101, 116, 114, 105, 99, 18, 13, 109,
        105, 99, 114, 111, 115, 101, 114, 118, 105, 99, 101, 115, 26, 4, 110, 111, 100, 101, 37, 0,
        0, 64, 65, 40, -71, 96)
    }
    "write a message with non-sequential field numbers to Protobuf" in {
      case class AnnotatedMessage(
          @pbIndex(1) a: String,
          @pbIndex(3) b: Int
      )
      val message = AnnotatedMessage("Hello", 3)
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111, 24, 3)
    }

    type Cop = Int :+: String :+: Boolean :+: CNil
    case class CoproductMessage(
        @pbIndex(1) a: Int,
        @pbIndex(3, 5, 7) b: Cop
    )
    "write a properly annotated message with a Coproduct field (1st branch)" in {
      val message = CoproductMessage(5, 9.inject[Cop])
      message.toPB shouldBe Array[Byte](8, 5, 24, 9)
    }
    "write a properly annotated message with a Coproduct field (2nd branch)" in {
      val message = CoproductMessage(5, "Hello".inject[Cop])
      message.toPB shouldBe Array[Byte](8, 5, 42, 5, 72, 101, 108, 108, 111)
    }
    "write a properly annotated message with a Coproduct field (3rd branch)" in {
      val message = CoproductMessage(5, true.inject[Cop])
      message.toPB shouldBe Array[Byte](8, 5, 56, 1)
    }
    "write a oneof field even if it has the default value" in {
      val message = CoproductMessage(5, "".inject[Cop])
      message.toPB shouldBe Array[Byte](8, 5, 42, 0)
    }
  }
}
