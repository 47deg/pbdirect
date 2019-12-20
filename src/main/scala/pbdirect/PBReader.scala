package pbdirect

import java.io.ByteArrayOutputStream

import cats.Functor
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.nat._
import enumeratum.values.{IntEnum, IntEnumEntry}

import scala.util.Try

trait PBMessageReader[A] {
  def read(bytes: Array[Byte]): A
}

object PBMessageReader {

  def instance[A](f: Array[Byte] => A): PBMessageReader[A] =
    new PBMessageReader[A] {
      override def read(bytes: Array[Byte]): A = f(bytes)
    }

  object collectFieldIndices extends Poly1 {
    implicit def annotatedCase[N <: Nat] = at[(Some[pbIndex], N)] {
      case (Some(annotation), _) => FieldIndex(annotation.first :: annotation.more.toList)
    }
    implicit def unannotatedCase[N <: Nat](implicit toInt: ToInt[N]) = at[(None.type, N)] {
      case (None, n) => FieldIndex(List(toInt() + 1))
    }
  }

  implicit def prodReader[A, R <: HList, Anns <: HList, ZWI <: HList, I <: HList](
      implicit
      gen: Generic.Aux[A, R],
      annotations: Annotations.Aux[pbIndex, A, Anns],
      zwi: ZipWithIndex.Aux[Anns, ZWI],
      indices: Mapper.Aux[collectFieldIndices.type, ZWI, I],
      reader: Lazy[PBProductReader[R, I]]): PBMessageReader[A] = instance { (bytes: Array[Byte]) =>
    val fieldIndices = annotations.apply.zipWithIndex.map(collectFieldIndices)
    //val bytes        = input.readByteArray()
    gen.from(reader.value.read(fieldIndices, bytes))
  }

  implicit val cnilReader: PBMessageReader[CNil] = instance { (bytes: Array[Byte]) =>
    throw new UnsupportedOperationException("Can't read CNil")
  }

  implicit def cconsReader[H, T <: Coproduct](
      implicit
      head: PBMessageReader[H],
      tail: Lazy[PBMessageReader[T]]): PBMessageReader[H :+: T] = instance { (bytes: Array[Byte]) =>
    Try {
      Inl(head.read(bytes))
    } getOrElse {
      Inr(tail.value.read(bytes))
    }
  }

  implicit def coprodReader[A, R <: Coproduct](
      implicit
      gen: Generic.Aux[A, R],
      repr: Lazy[PBMessageReader[R]]): PBMessageReader[A] = instance { (bytes: Array[Byte]) =>
    gen.from(repr.value.read(bytes))
  }

}

trait PBProductReader[R <: HList, I <: HList] {
  def read(indices: I, bytes: Array[Byte]): R
}

object PBProductReader {

  def instance[R <: HList, I <: HList](f: (I, Array[Byte]) => R): PBProductReader[R, I] =
    new PBProductReader[R, I] {
      def read(indices: I, bytes: Array[Byte]): R = f(indices, bytes)
    }

  implicit val hnilProductReader: PBProductReader[HNil, HNil] = PBProductReader.instance {
    (indices: HNil, bytes: Array[Byte]) =>
      HNil
  }

  implicit def consProductReader[H, T <: HList, IT <: HList](
      implicit
      headFieldReader: PBFieldReader[H],
      tail: Lazy[PBProductReader[T, IT]]): PBProductReader[H :: T, FieldIndex :: IT] =
    PBProductReader.instance { (indices: FieldIndex :: IT, bytes: Array[Byte]) =>
      headFieldReader.parse(indices.head.values.head, bytes) :: tail.value.read(indices.tail, bytes)
    }

}

trait PBScalarValueReader[A] {
  def read(input: CodedInputStream): A
}

trait PBScalarValueReaderImplicits_1 {

  def instance[A](f: CodedInputStream => A): PBScalarValueReader[A] =
    new PBScalarValueReader[A] {
      override def read(input: CodedInputStream): A = f(input)
    }

  implicit def embeddedMessageReader[A](
      implicit reader: PBMessageReader[A]): PBScalarValueReader[A] =
    instance { (input: CodedInputStream) =>
      val bytes = input.readByteArray()
      reader.read(bytes)
    }

}

trait PBScalarValueReaderImplicits extends PBScalarValueReaderImplicits_1 {

  implicit object BooleanReader$ extends PBScalarValueReader[Boolean] {
    override def read(input: CodedInputStream): Boolean = input.readBool()
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ByteReader$ extends PBScalarValueReader[Byte] {
    override def read(input: CodedInputStream): Byte = input.readInt32().toByte
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ShortReader$ extends PBScalarValueReader[Short] {
    override def read(input: CodedInputStream): Short = input.readInt32().toShort
  }
  implicit object IntReader$ extends PBScalarValueReader[Int] {
    override def read(input: CodedInputStream): Int = input.readInt32()
  }
  implicit object LongReader$ extends PBScalarValueReader[Long] {
    override def read(input: CodedInputStream): Long = input.readInt64()
  }
  implicit object FloatReader$ extends PBScalarValueReader[Float] {
    override def read(input: CodedInputStream): Float = input.readFloat()
  }
  implicit object DoubleReader$ extends PBScalarValueReader[Double] {
    override def read(input: CodedInputStream): Double = input.readDouble()
  }
  implicit object StringReader$ extends PBScalarValueReader[String] {
    override def read(input: CodedInputStream): String = input.readString()
  }
  implicit object BytesReader$ extends PBScalarValueReader[Array[Byte]] {
    override def read(input: CodedInputStream): Array[Byte] = input.readByteArray()
  }

  def apply[A: PBScalarValueReader]: PBScalarValueReader[A] = implicitly

  implicit object FunctorReader extends Functor[PBScalarValueReader] {
    override def map[A, B](reader: PBScalarValueReader[A])(f: A => B): PBScalarValueReader[B] =
      instance { (input: CodedInputStream) =>
        f(reader.read(input))
      }
  }

  implicit def enumReader[A](
      implicit
      values: Enum.Values[A],
      ordering: Ordering[A],
      reader: PBScalarValueReader[Int]): PBScalarValueReader[A] = instance {
    (input: CodedInputStream) =>
      Enum.fromInt[A](reader.read(input))
  }

  implicit def enumerationReader[E <: Enumeration](
      implicit
      reader: PBScalarValueReader[Int],
      gen: Generic.Aux[E, HNil]): PBScalarValueReader[E#Value] = instance {
    (input: CodedInputStream) =>
      val enum = gen.from(HNil)
      enum(reader.read(input))
  }

  implicit def enumeratumIntEnumEntryReader[E <: IntEnumEntry](
      implicit
      reader: PBScalarValueReader[Int],
      enum: IntEnum[E]): PBScalarValueReader[E] = instance { (input: CodedInputStream) =>
    enum.withValue(reader.read(input))
  }

  implicit def keyValuePairReader[K, V](
      implicit keyReader: PBScalarValueReader[K],
      valueReader: PBScalarValueReader[V]): PBScalarValueReader[(K, V)] = instance {
    (input: CodedInputStream) =>
      val bytes = input.readByteArray()
      val in    = CodedInputStream.newInstance(bytes)
      in.readTag()
      val key = keyReader.read(in)
      in.readTag()
      val value = valueReader.read(in)
      (key, value)
  }

}

object PBScalarValueReader extends PBScalarValueReaderImplicits

trait PBFieldReader[A] {
  def parse(index: Int, bytes: Array[Byte]): A
}

trait PBFieldReaderImplicits {
  def instance[A](f: (Int, Array[Byte]) => A): PBFieldReader[A] = new PBFieldReader[A] {
    override def parse(index: Int, bytes: Array[Byte]): A = f(index, bytes)
  }

  implicit def repeatedFieldReader[A](
      implicit reader: PBScalarValueReader[A]): PBFieldReader[List[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input       = CodedInputStream.newInstance(bytes)
      var done        = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                          => done = true
          case tag if (tag >> 3) == index => as ::= reader.read(input)
          case tag                        => input.skipField(tag)
        }
      }
      as.reverse
    }

  implicit def requiredFieldReader[A](implicit reader: PBScalarValueReader[A]): PBFieldReader[A] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input       = CodedInputStream.newInstance(bytes)
      var done        = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                          => done = true
          case tag if (tag >> 3) == index => as ::= reader.read(input)
          case tag                        => input.skipField(tag)
        }
      }
      as.head
    }

  implicit def optionalFieldReader[A](
      implicit parser: PBFieldReader[List[A]]): PBFieldReader[Option[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).lastOption
    }

  implicit def mapFieldReader[K, V](
      implicit parser: PBFieldReader[List[(K, V)]]): PBFieldReader[Map[K, V]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).toMap
    }

  implicit def collectionMapFieldReader[K, V](
      implicit parser: PBFieldReader[List[(K, V)]]): PBFieldReader[collection.Map[K, V]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).toMap
    }

  implicit def seqFieldReader[A](implicit parser: PBFieldReader[List[A]]): PBFieldReader[Seq[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes)
    }

}

object PBFieldReader extends PBFieldReaderImplicits
