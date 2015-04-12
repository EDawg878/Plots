name := "ScalaTest"

organization := "com.edawg878"

version := "1.0"

scalaVersion := "2.11.5"

retrieveManaged := true

resolvers += "spigot-repo" at "https://hub.spigotmc.org/nexus/content/groups/public/"

resolvers += "sonatype-oss-public" at "https://oss.sonatype.org/content/groups/public/"

resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

resolvers += "bukkit-repo" at "http://repo.bukkit.org/content/groups/public/"

resolvers += "sk89q-repo" at "http://maven.sk89q.com/repo/"

resolvers += Resolver.sonatypeRepo("public")

unmanagedJars in Compile += file("lib_unmanaged/scopt_2.11-3.3.0.jar")

libraryDependencies ++= Seq(
  "org.spigotmc" % "spigot-api" % "1.8-R0.1-SNAPSHOT",
  "net.md-5" % "bungeecord-api" % "1.8-SNAPSHOT",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23",
  "com.softwaremill.macwire" %% "macros" % "0.8.0",
  "com.softwaremill.macwire" %% "runtime" % "0.8.0",
  //"com.softwaremill.quicklens" %% "quicklens" % "1.2",
  "com.typesafe.akka" %% "akka-actor" % "2.3.6",
  "com.typesafe.akka" %% "akka-contrib" % "2.3.6",
  "com.typesafe.play" %% "play-json" % "2.3.4"
  //"com.github.scopt" %% "scopt" % "3.3.0"
)