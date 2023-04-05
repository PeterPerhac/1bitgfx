package com.perhac.utils.images.onebit

import com.perhac.utils.images.onebit.BlockColor._
import com.perhac.utils.images.onebit.OneBitCodec.cannotOverwriteExistingFile
import com.perhac.utils.images.onebit.animation.{Bounce, ImageGrabber, ScreenImageGrabber, WebcamImageGrabber}
import org.bytedeco.javacv.CanvasFrame

import java.awt.event.{MouseEvent, MouseListener}
import java.awt.image.BufferedImage
import java.awt.{Color, Toolkit}
import java.io.{ByteArrayOutputStream, DataOutputStream, FileInputStream, FileOutputStream}
import java.nio.file.Paths
import java.util.zip.{Deflater, DeflaterOutputStream}
import javax.imageio.ImageIO
import javax.swing.SwingUtilities.isLeftMouseButton
import javax.swing.WindowConstants
import javax.swing.event.MouseInputAdapter
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object OneBitEncoder {

  private val RECORDING_FPS                    = 30
  private var pixelClassifier: PixelClassifier = DefaultClassifier

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

  def createPreview(img: BufferedImage, midpoint: Midpoint): BufferedImage = {
    def pixelValue(color: Int): PixelValue = PixelValue(new Color(color).getRGBColorComponents(null).sum)
    for (x <- 0 until img.getWidth; y <- 0 until img.getHeight) {
      val pixelColour = pixelClassifier.classifyPixel(pixelValue(img.getRGB(x, y)), midpoint).getRGB
      img.setRGB(x, y, pixelColour)
    }
    img
  }

  //cycle backwards with right-click
  private def cycleThroughClassifiersOnMouseClick: MouseListener = new MouseInputAdapter {
    var idx: Int                            = 0
    val classifiers: Array[PixelClassifier] = Array(DefaultClassifier, ContouredClassifier, LowAndHigh)
    override def mouseReleased(e: MouseEvent): Unit = {
      def clamp(lo: Int, hi: Int)(n: Int): Int = if (n > hi) lo else if (n < lo) hi else n
      System.out.println()
      idx = clamp(0, classifiers.length - 1)(idx + (if (isLeftMouseButton(e)) 1 else -1))
      pixelClassifier = classifiers(idx)
      System.out.println(s"using $pixelClassifier")
    }
  }

  def setupPreviewWindow(grabber: ImageGrabber): CanvasFrame = {
    val frame = new CanvasFrame("1 Bit Animation Recorder (Preview)")
    frame.setCanvasSize(grabber.size.width, grabber.size.height)
    frame.setLocationRelativeTo(null) // center it on screen
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.getContentPane.getComponents.foreach { cmp =>
      cmp.addMouseListener(cycleThroughClassifiersOnMouseClick)
    }
    frame
  }

  def record(outPath: String, threshold: Option[Float], overwriteExisting: Boolean, webcam: Boolean): Unit = {
    val imageGrabber = if (webcam) new WebcamImageGrabber() else new ScreenImageGrabber()

    if (!Paths.get(outPath).toFile.exists() || overwriteExisting) {
      val out = new DataOutputStream(new FileOutputStream(outPath))
      out.writeByte(Bounce.atFps(RECORDING_FPS))
      val canvas: CanvasFrame = if (webcam) setupPreviewWindow(imageGrabber) else null
      try {
        var frameNo: Int = 0
        do {
          Thread.sleep(10) //force a slightly lower framerate
          frameNo = frameNo + 1
          val grabbedImage = imageGrabber.grab()
          // resize is still required because imageGrabber will grab a slightly bigger image than we ask it to
          val img =
            if (webcam) ImageUtils.resize(grabbedImage, imageGrabber.size.width, imageGrabber.size.height)
            else grabbedImage
          val midpoint = encodeImage(img, threshold, out, frameNo == 1)
          if (webcam) { canvas.showImage(createPreview(img, midpoint)) }
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
  ): Midpoint = {
    val (blocks, midpoint) = imageToBlocks(img, threshold)
    if (firstInSequence) { // width and height to be written to OUT only for the first image in the file
      val blockW = img.getWidth / 16
      val blockH = blocks.size / blockW
      out.write(Array[Byte](blockW.toByte, blockH.toByte))
    }
    serialize(blocks, out)
    midpoint
  }

  private def imageToBlocks(img: BufferedImage, threshold: Option[Float]): (ArrayBuffer[Block], Midpoint) = {
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
        classifier = this.pixelClassifier
      )

      blocks.addOne(makeBlock(rgbs))
    }
    (blocks, Midpoint(midpoint))
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

    val whitePixelCount = pixels.count(_ == White)

    if (whitePixelCount >= 251) { //almost all white (max 5 black pixels will be lost)
      WhiteBlock
    } else if (whitePixelCount < 6) { //almost all black (max 5 white pixels will be lost)
      BlackBlock
    } else {
      processBlock(pixels, new MixedColorBlockBuilder(pixels.head)).build()
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

    // we do NOT want to compress the block descriptors
    // so that they always take up exactly the same number of bytes for each frame in case of animated 1bp
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

    out.writeInt(compressedByteArrayOutputStream.size())
    compressedByteArrayOutputStream.writeTo(out)
  }

}
