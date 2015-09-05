import scala.annotation.tailrec

def toBinary(n: Int): String = convert(n, 2)

@tailrec
def convert(n: Int, b: Int, r: String = ""): String = {
  if (n == 0) r
  else convert(b / 2, b, b % 2 + r)
}

toBinary(25)

val hex = ('a' to 'f').zip(10 to 15).toMap

def toDecimal(s: String, m: Double, b: Int): Int = {
  if (s.isEmpty) m.toInt
  else {
    val n = if (hex.contains(s.head)) hex(s.head) else s.head
    toDecimal(s.tail, m + Character.getNumericValue(n) * math.pow(b, s.length - 1), b)
  }
}

toDecimal("D1CE", 0, 16)

