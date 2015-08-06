import com.edawg878.bukkit.plot.PlotId
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global


val set = mutable.Set[(Int, Int)]()

@tailrec
def auto(n: Int, x: Int, z: Int): Unit = {
  println(s"n=$n ($x;$z)")
  val id = (x, z)
  if (set contains id)
    println(s"Already contained ($x,$z)")
  else
    set += id
  if (z < n) auto(n, x, z + 1)
  else if (x < n) auto(n, x + 1, -n)
  else auto(n + 1, -(n + 1), -(n + 1))
}

auto(0,0,0)

/*
Seq(1,2,3).map{e =>
  val c = 1
  val b = 3
  b
}
*/

//hello Eric my name is anne marie well this is stuff i tupe all of the time


import scala.concurrent.duration._

val p = Promise[Int]()
val f = p.future

p success(10)

Await.result(f, 10 seconds)