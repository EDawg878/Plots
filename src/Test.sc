import java.util.UUID

import scala.annotation.tailrec

private def toBytes(id: UUID): Array[Byte] = {
  val shifts = Array.range(0, 64, 8).reverse
  def split(word: Long) =
    shifts map(word >>> _ ) map(_.asInstanceOf[Byte])
  split(id.getMostSignificantBits) ++ split(id.getLeastSignificantBits)
}
private def fromBytes(data: Array[Byte]): UUID = {
  @tailrec
  def read(r: Range, acc: Long = 0): Long = {
    if (r.head == r.last) acc
    else read(r.drop(1), (acc << 8) | (data(r.head) & 0xff))
  }
  new UUID(read(0 to 8), read(8 to 16))
}
val id = UUID.randomUUID()
val data = toBytes(id)
val recover = fromBytes(data)
toBytes(recover)
0 to 8