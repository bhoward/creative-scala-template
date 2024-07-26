import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.image.syntax.core.*
import doodle.core.font.*
import doodle.java2d.*
import doodle.reactor.*
import scala.concurrent.duration.*
import cats.effect.unsafe.implicits.global

// To use this example:
//
// 1. run `sbt`
// 2. run the `run` command within `sbt`
object Example {
  val image =
    Image
      .circle(100)
      .fillColor(Color.red)
      .on(Image.circle(200).fillColor(Color.aquamarine))
      .on(Image.circle(300).fillColor(Color.steelBlue))

  val animation =
    Reactor
      .init(0.degrees)
      .withOnTick(a => a + 1.degrees)
      .withStop(a => a > 360.degrees)
      .withTickRate(20.millis)
      .withRender { a =>
        val location = Point(200, a)
        val planet = Image.circle(40.0).noStroke.fillColor(Color.seaGreen)
        val moon = Image
          .circle(10.0)
          .noStroke
          .fillColor(Color.slateGray)
          .at(Point(60, a * 5))

        moon.on(planet).at(location)
      }

  val frame = Frame.default.withSize(600, 600).withCenterAtOrigin

  @main def go(): Unit = {
    image.draw()

    // Comment out the above and uncomment the below to display the animation
    // animation.run(frame)
  }
}
