package com.perhac.utils.images.onebit

import com.perhac.utils.images.onebit.BlockColor._
import com.perhac.utils.images.onebit.OneBitCodec.cannotOverwriteExistingFile
import com.perhac.utils.images.onebit.animation.{Bounce, ScreenImageGrabber, WebcamImageGrabber}

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, DataOutputStream, FileInputStream, FileOutputStream}
import java.nio.file.Paths
import java.util.zip.{Deflater, DeflaterOutputStream}
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object OneBitEncoder {

  private val RECORDING_FPS = 30

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
    averages.sum / averages.size
  }

  def record(outPath: String, threshold: Option[Float], overwriteExisting: Boolean, webcam: Boolean): Unit = {
    val imageGrabber = if (webcam) new WebcamImageGrabber() else new ScreenImageGrabber()

    if (!Paths.get(outPath).toFile.exists() || overwriteExisting) {
      val out = new DataOutputStream(new FileOutputStream(outPath))
      out.writeByte(Bounce.atFps(RECORDING_FPS))
      try {
        var frameNo: Int = 0
        do {
          frameNo = frameNo + 1
          val grabbedImage = imageGrabber.grab()
          val img          = ImageUtils.resize(grabbedImage, 432, 270)
          encodeImage(img, threshold, out, frameNo == 1)
          print(f"\rrecorded frames: $frameNo%d (estimated ${frameNo.toDouble / RECORDING_FPS}%2.2fs)")
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
      val out = new DataOutputStream(new FileOutputStream(outPath))
      try {
        out.write(Array[Byte](0.toByte)) //animation byte, just a zero if no animation
        encodeImage(ImageIO.read(new FileInputStream(inPath)), threshold, out)
        System.out.println("output file saved to: " + outPath)
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
      out: DataOutputStream,
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

  private def imageToBlocks(img: BufferedImage, threshold: Option[Float]): ArrayBuffer[Block] = {
    val blockW = img.getWidth / 16
    val blockH = img.getHeight / 16

    val blocks: ArrayBuffer[Block] = new ArrayBuffer[Block](blockW * blockH)
    val midpoint: Float            = threshold.getOrElse(calculateMidPoint(img))
    //for each block (16x16 rectangle within the picture)
    for (rowIdx <- 0 until blockH; colIdx <- 0 until blockW) {
      val rgbs = for {
        y <- 0 to 15
        x <- 0 to 15
      } yield fromAwtColor(
        color = new Color(img.getRGB(colIdx * 16 + x, rowIdx * 16 + y)),
        midPoint = midpoint,
        classifier = LowAndHigh //TODO plug one in based on command line switch / option
      )

      blocks.addOne(makeBlock(rgbs))
    }
    blocks
  }

  private def makeBlock(pixels: IndexedSeq[BlockColor]): Block = {
    @tailrec
    def processBlock(
        pixels: IndexedSeq[BlockColor],
        builder: MixedColorBlockBuilder
    ): MixedColorBlockBuilder =
      if (pixels.nonEmpty) {
        val firstElement = pixels.head
        processBlock(pixels.dropWhile(_ == firstElement), builder.addLength(pixels.takeWhile(_ == firstElement).size))
      } else builder

    if (pixels.forall(_ == White)) { //all white
      WhiteBlock
    } else if (pixels.contains(White)) { //mixed b/w
      processBlock(pixels, new MixedColorBlockBuilder(pixels.head)).build()
    } else {
      BlackBlock
    }
  }

  private def serialize(
      blocks: ArrayBuffer[Block],
      out: DataOutputStream
  ): Unit = {
    def blockDescriptorBytes: Array[Byte] =
      blocks
        .map(_.descriptor)
        .grouped(4)
        .map(_.toList match {
          case one :: two :: three :: four :: Nil => (one << 6 | two << 4 | three << 2 | four).toByte
          case one :: two :: three :: Nil         => (one << 6 | two << 4 | three << 2).toByte
          case one :: two :: Nil                  => (one << 6 | two << 4).toByte
          case one :: Nil                         => (one << 6).toByte
          case Nil                                => sys.error("grouping must not produce empty list")
          case _                                  => sys.error("more??? can't have more than 4 things when grouped by 4")
        })
        .toArray

    out.write(blockDescriptorBytes)

    val compressor: Deflater            = new Deflater(Deflater.BEST_COMPRESSION, true)
    val compressedByteArrayOutputStream = new ByteArrayOutputStream(1024)
    val compressedBlockData: DeflaterOutputStream =
      new DeflaterOutputStream(compressedByteArrayOutputStream, compressor)

    try {
      blocks.foreach {
        case block: MixedColorBlock =>
          compressedBlockData.write(block.lengths.map(_.toByte).toArray)
        case _ => //do nothing
      }
    } finally {
      compressedBlockData.close()
    }

    val deflatedBytesCount = compressedByteArrayOutputStream.size()
    out.writeInt(deflatedBytesCount)                                              //writes the NUMBER of bytes in the data block
    out.write(compressedByteArrayOutputStream.toByteArray, 0, deflatedBytesCount) //writes the data block itself
  }

}
