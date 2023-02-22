package com.perhac.utils.images.onebit

import com.perhac.utils.images.onebit.BlockColor.fromAwtColor
import com.perhac.utils.images.onebit.OneBitCodec.cannotOverwriteExistingFile
import com.perhac.utils.images.onebit.animation.Loop
import org.bytedeco.javacv.{Java2DFrameConverter, OpenCVFrameGrabber}

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, FileInputStream, FileOutputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.file.Paths
import java.util.concurrent.TimeoutException
import java.util.zip.Deflater
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object OneBitEncoder {

  private def calculateMidPoint(img: BufferedImage): Float = {
    val averages = for {
      x <- 0 until img.getWidth
      y <- 0 until img.getHeight
    } yield {
      val clr   = img.getRGB(x, y)
      val red   = (clr & 0x00ff0000) >> 16
      val green = (clr & 0x0000ff00) >> 8
      val blue  = clr & 0x000000ff
      (red + green + blue).toFloat / (3 * 256)
    }
    val midpoint = averages.toList.sum / averages.size
    System.out.println("auto threshold: " + midpoint)
    midpoint
  }

  def record(outPath: String, threshold: Option[Float], overwriteExisting: Boolean): Unit = {
    val grabber = new OpenCVFrameGrabber(0)
    println("Starting Webcam...")
    val futureGrabberResult = Future { grabber.start() }
    try {
      Await.ready(futureGrabberResult, 5.seconds)
    } catch {
      case _: TimeoutException =>
        System.err.println("Reached 5s timeout while waiting for Webcam to initialise")
        sys.exit(3)
    }
    println("Webcam started.")
    val paintConverter = new Java2DFrameConverter()
    if (!Paths.get(outPath).toFile.exists() || overwriteExisting) {
      val out = new FileOutputStream(outPath)
      try {
        var frameNo: Int = 0
        do {
          frameNo = frameNo + 1
          out.write(Array[Byte](Loop.atFps(32)))
          val img = ImageUtils.resize(paintConverter.convert(grabber.grab()), 768, 480)
          encodeImage(img, threshold, out, frameNo == 1)
          println(s"$frameNo of 500")
        } while (!Thread.interrupted())
      } finally {
        out.close()
      }
    } else {
      cannotOverwriteExistingFile()
    }
  }

  def encodeSingleFile(inPath: String, outPath: String, threshold: Option[Float], overwriteExisting: Boolean): Unit = {
    if (!Paths.get(outPath).toFile.exists() || overwriteExisting) {
      val out = new FileOutputStream(outPath)
      try {
        out.write(Array[Byte](0.toByte)) //animation byte, just a zero if no animation
        encodeImage(ImageIO.read(new FileInputStream(inPath)), threshold, out)
        System.out.println("output file saved to:" + outPath)
      } finally {
        out.close()
      }
    } else {
      cannotOverwriteExistingFile()
    }
  }

  private def encodeImage(
      img: BufferedImage,
      threshold: Option[Float],
      out: OutputStream,
      firstInSequence: Boolean = true
  ): Unit = {
    val blocks = imageToBlocks(img, threshold)
    if (firstInSequence) { // width and height to be written to OUT only for the first image in the file
      val blockW = img.getWidth / 16
      val blockH = blocks.size / blockW
      out.write(Array[Byte](blockW.toByte, blockH.toByte))
    }
    serialize(blocks, out)
  }

  private def imageToBlocks(img: BufferedImage, threshold: Option[Float]): List[Block] = {
    val blockW = img.getWidth / 16
    val blockH = img.getHeight / 16

    val blocks: ArrayBuffer[Block] = new ArrayBuffer[Block](blockW * blockH)
    val midpoint: Float            = threshold.getOrElse(calculateMidPoint(img))
    //for each block (16x16 rectangle within the picture)
    for (rowIdx <- 0 until blockH; colIdx <- 0 until blockW) {
      val rgbs = for {
        y <- (0 to 15).toList
        x <- (0 to 15).toList
      } yield fromAwtColor(
        color = new Color(img.getRGB(colIdx * 16 + x, rowIdx * 16 + y)),
        midPoint = midpoint
      )

      blocks.addOne(makeBlock(rgbs))
    }
    blocks.toList
  }

  private def makeBlock(pixels: List[BlockColor]): Block = {
    @tailrec
    def processBlock(
        pixels: List[BlockColor],
        builder: MixedColorBlockBuilder
    ): MixedColorBlockBuilder =
      pixels match {
        case h :: _ => processBlock(pixels.dropWhile(_ == h), builder.addLength(pixels.takeWhile(_ == h).size))
        case Nil    => builder
      }

    if (pixels.forall(_ == White)) { //all white
      WhiteBlock
    } else if (pixels.contains(White)) { //mixed b/w
      processBlock(pixels, new MixedColorBlockBuilder(pixels.head)).build()
    } else {
      BlackBlock
    }
  }

  private def serialize(
      blocks: List[Block],
      out: OutputStream
  ): Unit = {
    def blockDescriptorBytes: Array[Byte] =
      blocks
        .map(_.descriptor)
        .grouped(4)
        .map({
          case one :: two :: three :: four :: Nil => (one << 6 | two << 4 | three << 2 | four).toByte
          case one :: two :: three :: Nil         => (one << 6 | two << 4 | three << 2).toByte
          case one :: two :: Nil                  => (one << 6 | two << 4).toByte
          case one :: Nil                         => (one << 6).toByte
          case Nil                                => sys.error("grouping must not produce empty list")
          case _                                  => sys.error("more??? can't have more than 4 things when grouped by 4")
        })
        .toArray

    out.write(blockDescriptorBytes)
    val expectedBlockDataSize =
      blocks.count(_.isInstanceOf[MixedColorBlock]) * 32 //rough guess at initial byte array size needed
    val blockData: ByteArrayOutputStream = new ByteArrayOutputStream(expectedBlockDataSize)
    blocks.foreach {
      case block: MixedColorBlock =>
        blockData.write(block.lengths.map(_.toByte).toArray)
      case _ => //do nothing
    }
    val output: Array[Byte]  = new Array(blockData.size())
    val compressor: Deflater = new Deflater()
    compressor.setInput(blockData.toByteArray)
    compressor.finish()
    val deflatedBytesCount = compressor.deflate(output)
    compressor.end()
    out.write(ByteBuffer.allocate(4).putInt(deflatedBytesCount).array())
    out.write(output, 0, deflatedBytesCount)
  }

}
