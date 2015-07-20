import java.util.UUID

import reactivemongo.bson.BSONBinary

import scala.annotation.tailrec

def toBytes(id: UUID): Array[Byte] = {
  val shifts = Array.range(0, 64, 8).reverse
  def split(bits: Long) =
    shifts map (bits >>> _) map (_.asInstanceOf[Byte])
  split(id.getMostSignificantBits) ++ split(id.getLeastSignificantBits)
}

def fromBytes(data: Array[Byte]): UUID = {
  @tailrec def read(r: Range, acc: Long = 0): Long = {
    if (r.length == 1) acc
    else read(r.drop(1), (acc << 8) | (data(r.head) & 0xff))
  }
  new UUID(read(0 to 8), read(8 to 16))
}

val id = UUID.randomUUID()
val b = toBytes(id)
val nid = fromBytes(b)
id == nid