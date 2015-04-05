import java.util.UUID

// Existing 3rd-party java implementation, cannot change
case class ExamplePlayer1() {

  def getUniqueId = UUID.randomUUID()

}

class ExampleServer1 {

  def getPlayer(id: UUID): ExamplePlayer1 = null // returns either null or ExamplePlayer1

}
//


// Another 3rd-party java implementation, cannot change
case class ExamplePlayer2() {

  def getUUID = UUID.randomUUID()

}

class ExampleServer2 {

  def getPlayer(id: UUID): ExamplePlayer2 = null // returns either null or ExamplePlayer2

}
//

// Those APIs are verry similar, let's use value classes to "wrap" them

// Wrapper traits
trait Server {

  def getPlayer(id: UUID): Option[Player]

}

trait Player {

  def getId: UUID

}
//

// Value classes to wrap implementation #1
implicit class PlayerWrapper1(val p: ExamplePlayer1) extends Player {

  override def getId = p.getUniqueId

}

implicit class ServerWrapper1(val server: ExampleServer1) extends Server {

  override def getPlayer(id: UUID): Option[Player] = {
    // Doesn't work properly
    // Option(null) => Some()
    val p = server.getPlayer(id)
    if (p)
  }

}

// Value classes to wrap implementation #2
implicit class PlayerWrapper2(val p: ExamplePlayer2) extends Player {

  override def getId = p.getUUID

}

implicit class ServerWrapper2(val server: ExampleServer2) extends Server {

  override def getPlayer(id: UUID): Option[Player] = {
    // But this works properly??
    val p = server.getPlayer(id)
    if (p == null) None else Some(p)
  }

}

def run(server: Server): Unit = {
  val id = UUID.randomUUID()
  val maybePlayer = server.getPlayer(id)// returns null (i.e. player wasn't found)
  println(maybePlayer.isDefined)
}


run(new ExampleServer1) // prints 'true' - Why is that, Shouldn't Option(null) return None???
run(new ExampleServer2) // prints 'false' - Expected output