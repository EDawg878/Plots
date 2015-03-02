name := "ScalaTest"

organization := "com.edawg878"

version := "1.0"

scalaVersion := "2.11.5"

resolvers += "spigot-repo" at "https://hub.spigotmc.org/nexus/content/groups/public/"

resolvers += "sonatype-oss-public" at "https://oss.sonatype.org/content/groups/public/"

resolvers += "bukkit-repo" at "http://repo.bukkit.org/content/groups/public/"

resolvers += "sk89q-repo" at "http://maven.sk89q.com/repo/"

libraryDependencies ++= Seq(
  "org.spigotmc" % "spigot-api" % "1.8-R0.1-SNAPSHOT",
  "net.md-5" % "bungeecord-api" % "1.8-SNAPSHOT"
)