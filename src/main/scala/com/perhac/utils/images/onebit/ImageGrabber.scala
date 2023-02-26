package com.perhac.utils.images.onebit

import org.bytedeco.javacv.{Java2DFrameConverter, OpenCVFrameGrabber}

import java.awt.{Rectangle, Robot, Toolkit}
import java.awt.image.BufferedImage
import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

trait ImageGrabber {

  def grab(): BufferedImage

}

class WebcamImageGrabber() extends ImageGrabber {
  val grabber = new OpenCVFrameGrabber(0)
  println("Starting Webcam...")
  private val futureGrabberResult = Future { grabber.start() }
  try {
    Await.ready(futureGrabberResult, 5.seconds)
  } catch {
    case _: TimeoutException =>
      System.err.println("Reached 5s timeout while waiting for Webcam to initialise")
      sys.exit(3)
  }
  private val paintConverter = new Java2DFrameConverter()

  override def grab(): BufferedImage = paintConverter.convert(grabber.grab())
}

class ScreenImageGrabber() extends ImageGrabber {
  private val screenSize = Toolkit.getDefaultToolkit.getScreenSize
  private val rectangle  = new Rectangle(0, 0, screenSize.width, screenSize.height)
  private val robot      = new Robot()

  override def grab(): BufferedImage = {
    robot.createScreenCapture(rectangle)
  }
}
