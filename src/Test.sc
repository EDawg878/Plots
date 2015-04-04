import scala.collection.generic.Sorted

val vals = Vector(0,10,30,50,100).sorted

def promote(num: Int): Int = vals.find(x => x > num) getOrElse num
def demote(num: Int): Int = vals.reverse.find(x => x < num) getOrElse num

demote(0)