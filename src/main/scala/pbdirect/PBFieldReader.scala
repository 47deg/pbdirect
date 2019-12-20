package pbdirect

import com.google.protobuf.CodedInputStream

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
