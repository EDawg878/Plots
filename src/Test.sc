/*import scala.annotation.tailrec

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
*/

/*

import scala.collection.mutable
import scala.io.Source.stdin

val ln = "5 5\n11011\n10101\n01110\n10101\n11011".split("\n").toIterator

val in = ln.toIndexedSeq
val lines = in.tail
val config = in.head.split(" ").map(_.toInt)
val (r, c) = (config(0), config(1))
case class Coord(r: Int, c: Int)
val map = new mutable.HashMap[Coord, Boolean]()

for (x <- 0 until c) {
  for (y <- 0 until r) {
    map.put(Coord(x, y), lines(y).charAt(x) match {
      case '1' => true
      case '0' => false
      case t => throw new RuntimeException(s"Invalid input $t")
    })
  }
}

map
*/

/*
import java.util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source.stdin

val st = "22 4 0 6\n3 2\n4 7\n13 6\n10 0".split("\n").toIterator
val ln = st.next().split(" ").iterator
val (f, e, a, b) = (ln.next().toInt, ln.next().toInt, ln.next().toInt, ln.next().toInt)
case class Elevator(x: Int, y: Int) {
  def floors: Set[Int] = (y to 100 by x).toSet
  def highest: Int = floors.max
}
val elevators = (for {
  i <- 0 until e
  split = st.next().split(" ").map(_.toInt)
} yield (Elevator(split(0), split(1))))

val floors = elevators.map(_.floors).reduce(_ union _)
// f = number of floors
// e = number of elevators, stop every x floor
// elevator cannot go lower than base floor y
// a = start floor
// b = end floor

def isPossible(currentFloor: Int, dest: Int): Int = {
  if (currentFloor >= dest) 1
  else {
    elevators.filter(e => e.floors.contains(currentFloor + 1))
      .flatMap(e => (e.y to e.highest))
      .find(isPossible(_, dest) == 1)
      .fold(0)(e => 1)
  }
}

isPossible(a, b)
*/

def capitalize(s: String) = s.charAt(0).toUpper + s.substring(1).toLowerCase

capitalize("GABRIELE")