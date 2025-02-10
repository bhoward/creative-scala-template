scalaVersion := "3.6.3"

console / initialCommands := """
      |import doodle.core.*
      |import doodle.image.*
      |import doodle.image.syntax.all.*
      |import doodle.image.syntax.core.*
      |import doodle.core.font.*
      |import doodle.java2d.*
      |import cats.effect.unsafe.implicits.global
    """.trim.stripMargin

libraryDependencies ++= Seq(
  "org.creativescala" %% "doodle" % "0.27.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % "test"
)
