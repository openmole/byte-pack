
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.11.0")

addSbtPlugin("com.github.sbt" % "sbt-release" % "1.1.0")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.0")

resolvers += Resolver.sonatypeRepo("public")

