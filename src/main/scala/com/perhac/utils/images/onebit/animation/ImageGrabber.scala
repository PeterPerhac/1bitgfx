package com.perhac.utils.images.onebit.animation

import org.bytedeco.javacv.FrameGrabber.ImageMode
import org.bytedeco.javacv.{Java2DFrameConverter, OpenCVFrameGrabber}

import java.awt.image.BufferedImage
import java.awt.{Rectangle, Robot, Toolkit}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, TimeoutException}

trait ImageGrabber {

  def grab(): BufferedImage

}

class WebcamImageGrabber() extends ImageGrabber {
  val grabber = new OpenCVFrameGrabber(0)
  grabber.setImageMode(ImageMode.GRAY)
  //these exact dimensions will be ignored, and the nearest supported resolution of 640x480 will be grabbed
  grabber.setImageWidth(432)
  grabber.setImageHeight(270)
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

  override def grab(): BufferedImage =
    paintConverter.convert(grabber.grab())

}

class ScreenImageGrabber() extends ImageGrabber {
  private val screenSize = Toolkit.getDefaultToolkit.getScreenSize
  private val rectangle  = new Rectangle(100, 200, screenSize.width/3, screenSize.height/2)
  private val robot      = new Robot()

  override def grab(): BufferedImage = {
    robot.createScreenCapture(rectangle)
  }
}
