package pbdirect

import org.scalatest.flatspec._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.Prop._
import enumeratum.values.IntEnumEntry
import enumeratum.values.IntEnum

sealed abstract class Status(val value: Int) extends IntEnumEntry
object Status extends IntEnum[Status] {
  case object Running extends Status(0)
  case object Stopped extends Status(5)
  val values = findValues
}

class RoundTripSpec extends AnyFlatSpec with Checkers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)

  "round trip to protobuf and back" should "be an identity" in check {
    // TODO the test is failing because e.g. Some(false) gets turned into None.
    // Need to decide what the correct behaviour is and adjust either the test
    // or the implementation.
    pending

    forAllNoShrink { (message: MessageThree) =>
      val roundtripped = message.toPB.pbTo[MessageThree]

      // arrays use reference equality :(
      s"message after roundtrip = $roundtripped" |: all(
        "byte array" |: roundtripped.bytes.toList === message.bytes.toList,
        "rest of message" |: roundtripped.copy(bytes = message.bytes) === message
      )
    }
  }

  case class EmptyMessage()

  case class MessageOne(
      @pbIndex(1) optionalEmpty: Option[EmptyMessage],
      @pbIndex(2) boolean: Boolean,
      @pbIndex(3) repeatedFloat: List[Float]
  )

  case class MessageTwo(
      @pbIndex(5) int: Int,
      @pbIndex(10) string: String,
      @pbIndex(15) emptyMessage: EmptyMessage,
      @pbIndex(20) nestedMessage: MessageOne
  )

  case class MessageThree(
      @pbIndex(2) int: Int,
      @pbIndex(4) optionalInt: Option[Int],
      @pbIndex(6) boolean: Boolean,
      @pbIndex(8) optionalBoolean: Option[Boolean],
      @pbIndex(10) double: Double,
      @pbIndex(12) float: Float,
      @pbIndex(14) long: Long,
      @pbIndex(16) string: String,
      @pbIndex(18) repeatedString: List[String],
      @pbIndex(20) enum: Status,
      @pbIndex(22) repeatedEnum: List[Status],
      @pbIndex(24) optionalEnum: Option[Status],
      @pbIndex(26) byte: Byte,
      @pbIndex(28) short: Short,
      @pbIndex(30) bytes: Array[Byte],
      @pbIndex(32) intStringMap: Map[Int, String],
      @pbIndex(34) stringBoolListMap: Map[String, List[Boolean]],
      @pbIndex(36) nestedMessage: MessageTwo,
      @pbIndex(38) repeatedNestedMessage: List[MessageTwo],
      @pbIndex(40) intMessageMap: Map[Int, MessageTwo]
  )

}
