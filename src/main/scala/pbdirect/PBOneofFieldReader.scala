package pbdirect

import shapeless._

trait PBOneofFieldReader[A <: Coproduct] {
  def read(indices: List[Int], bytes: Array[Byte]): A
}

trait PBOneofFieldReaderImplicits {
  def instance[A <: Coproduct](f: (List[Int], Array[Byte]) => A): PBOneofFieldReader[A] =
    new PBOneofFieldReader[A] {
      override def read(indices: List[Int], bytes: Array[Byte]): A = f(indices, bytes)
    }

  implicit val cnilReader: PBOneofFieldReader[CNil] = instance(
    (_, _) => throw new IllegalStateException("Cannot read CNil"))

  implicit def cconsReader[H, T <: Coproduct](
      implicit
      headReader: PBFieldReader[Option[H]],
      tailReader: Lazy[PBOneofFieldReader[T]]): PBOneofFieldReader[H :+: T] =
    instance { (indices: List[Int], bytes: Array[Byte]) =>
      headReader
        .read(indices.head, bytes)
        .map(Inl(_))
        .getOrElse(Inr(tailReader.value.read(indices.tail, bytes)))
    }

}

object PBOneofFieldReader extends PBOneofFieldReaderImplicits {
  def apply[A <: Coproduct: PBOneofFieldReader]: PBOneofFieldReader[A] = implicitly
}
