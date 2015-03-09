name := "ScalaTest"

organization := "com.edawg878"

version := "1.0"

scalaVersion := "2.11.5"

resolvers += "spigot-repo" at "https://hub.spigotmc.org/nexus/content/groups/public/"

resolvers += "sonatype-oss-public" at "https://oss.sonatype.org/content/groups/public/"

resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "bukkit-repo" at "http://repo.bukkit.org/content/groups/public/"

resolvers += "sk89q-repo" at "http://maven.sk89q.com/repo/"

libraryDependencies ++= Seq(
  "org.spigotmc" % "spigot-api" % "1.8-R0.1-SNAPSHOT",
  "net.md-5" % "bungeecord-api" % "1.8-SNAPSHOT",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.reactivemongo" %% "reactivemongo" % "0.10.5.0.akka23",
  "com.softwaremill.macwire" %% "macros" % "0.8.0",
  "com.softwaremill.macwire" %% "runtime" % "0.8.0"
)