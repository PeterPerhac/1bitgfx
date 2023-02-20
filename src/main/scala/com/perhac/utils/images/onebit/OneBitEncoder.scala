package com.perhac.utils.images.onebit

import com.perhac.utils.images.onebit.BlockColor.fromAwtColor
import com.perhac.utils.images.onebit.OneBitCodec.cannotOverwriteExistingFile

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, FileInputStream, FileOutputStream, OutputStream}
import java.nio.file.Paths
import java.util.zip.Deflater
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object OneBitEncoder {

  def calculateMidPoint(img: BufferedImage): Float = {
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

  def encode(inPath: String, outPath: String, threshold: Option[Float], overwriteExisting: Boolean): Unit = {
    val img: BufferedImage = javax.imageio.ImageIO.read(new FileInputStream(inPath))
    val blockW             = img.getWidth / 16
    val blockH             = img.getHeight / 16

    val blocks: ArrayBuffer[Block] = new ArrayBuffer[Block](blockW * blockH)
    val midpoint: Float            = threshold.getOrElse(calculateMidPoint(img))
    //foreach block
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

    if (!Paths.get(outPath).toFile.exists() || overwriteExisting) {
      val out = new FileOutputStream(outPath)
      try {
        serialize(blocks.toList, blockW, out)
        System.out.println("output file saved to:" + outPath)
      } finally {
        out.close()
      }
    } else {
      cannotOverwriteExistingFile()
    }

  }

  @tailrec
  def processBlock(
      pixels: List[BlockColor],
      builder: MixedColorBlockBuilder
  ): MixedColorBlockBuilder =
    pixels match {
      case h :: _ => processBlock(pixels.dropWhile(_ == h), builder.addLength(pixels.takeWhile(_ == h).size))
      case Nil    => builder
    }

  def makeBlock(pixels: List[BlockColor]): Block =
    if (pixels.forall(_ == White)) { //all white
      WhiteBlock
    } else if (pixels.contains(White)) { //mixed b/w
      processBlock(pixels, new MixedColorBlockBuilder(pixels.head)).build()
    } else {
      BlackBlock
    }

  def blockDescriptorBytes(blocks: List[Block]): Array[Byte] =
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

  def serialize(
      blocks: List[Block],
      widthInBlocksWord: Int, //max a Word tho, 2 bytes, soz
      out: OutputStream
  ): Unit = {
    val heightInBlocks = blocks.size / widthInBlocksWord
    out.write(Array[Byte](widthInBlocksWord.toByte, heightInBlocks.toByte))
    out.write(blockDescriptorBytes(blocks))
    val expectedBlockDataSize =
      blocks.count(_.isInstanceOf[MixedColorBlock]) * 32 //rough guess at initial byte array size needed
    val blockData: ByteArrayOutputStream = new ByteArrayOutputStream(expectedBlockDataSize)
    blocks.foreach {
      case block: MixedColorBlock =>
        blockData.write(block.lengths.map(_.toByte).toArray)
      case _ => //do nothing
    }
    val output: Array[Byte]  = new Array(blockData.size())
    val compresser: Deflater = new Deflater()
    compresser.setInput(blockData.toByteArray)
    compresser.finish()
    val deflatedBytes = compresser.deflate(output)
    compresser.end()
    out.write(output, 0, deflatedBytes)
  }

}
