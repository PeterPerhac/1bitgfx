package com.perhac.utils.images.onebit

import com.perhac.utils.images.onebit.BlockColor.fromAwtColor

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{FileInputStream, FileOutputStream, OutputStream}
import java.nio.ByteBuffer
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object OneBitEncoder {

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
      })
      .toArray

  def serialize(
      blocks: List[Block],
      widthInBlocksWord: Int, //max a Word tho, 2 bytes, soz
      out: OutputStream
  ): Unit = {
    val heightInBlocks = blocks.size / widthInBlocksWord
    out.write(ByteBuffer.allocate(4).putInt(widthInBlocksWord << 16 | heightInBlocks).array)
    out.write(blockDescriptorBytes(blocks))
    blocks.foreach({
      case block: MixedColorBlock =>
        out.write(block.lengths.map(_.toByte).toArray)
      case _ => //do nothing
    })
  }

  def main(args: Array[String]): Unit = {
    val img: BufferedImage = javax.imageio.ImageIO.read(
      args.headOption.fold(this.getClass.getClassLoader.getResourceAsStream("image.png"))(new FileInputStream(_))
    )
    val blockW = img.getWidth / 16
    val blockH = img.getHeight / 16

    val blocks: ArrayBuffer[Block] = new ArrayBuffer[Block](blockW * blockH)

    //foreach block
    for (rowIdx <- 0 until blockH; colIdx <- 0 until blockW) {
      val rgbs = for {
        y <- (0 to 15).toList
        x <- (0 to 15).toList
      } yield fromAwtColor(
        color = new Color(img.getRGB(colIdx * 16 + x, rowIdx * 16 + y)),
        midPoint = 2.0f
      )

      blocks.addOne(makeBlock(rgbs))
    }
    val out = new FileOutputStream(args.drop(1).headOption.getOrElse("encodedImage.1bp"))
    try {
      serialize(blocks.toList, blockW, out)
    } finally {
      out.close()
    }

  }

}
