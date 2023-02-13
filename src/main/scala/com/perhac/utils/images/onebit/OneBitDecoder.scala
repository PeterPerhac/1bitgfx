package com.perhac.utils.images.onebit

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}
import java.io.{FileInputStream, FileOutputStream, InputStream}
import java.nio.ByteBuffer
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object OneBitDecoder {

  private case class BlockWithCoordinates(block: Block, colIdx: Int, rowIdx: Int) {
    override def toString: String = s"${block.toString} at [$colIdx, $rowIdx]"
  }

  def unpack(bytes: Array[Byte]): List[Block] = {
    val buf: ArrayBuffer[Block] = new ArrayBuffer[Block](bytes.length * 4) //4 descriptors per byte
    bytes.foreach { b =>
      buf.addOne(Block.fromDescriptor(((b & 0xc0) >>> 6).toByte))
      buf.addOne(Block.fromDescriptor(((b & 0x30) >>> 4).toByte))
      buf.addOne(Block.fromDescriptor(((b & 0xc) >>> 2).toByte))
      buf.addOne(Block.fromDescriptor((b & 0x3).toByte))
    }
    buf.toList
  }

  private def resolveCoordinates(blockDescriptors: List[Block], blockW: Int): List[BlockWithCoordinates] = {
    def coordinatesByIndex(i: Int): (Int, Int) =
      i % blockW -> i / blockW

    blockDescriptors.zipWithIndex.map { case (block, idx) =>
      val (cidx, ridx) = coordinatesByIndex(idx)
      BlockWithCoordinates(block = block, colIdx = cidx, rowIdx = ridx)
    }
  }

  def paintLengths(
      gfx: Graphics,
      b: BlockWithCoordinates,
      ls: List[Int],
      initialColorWhite: Boolean
  ): Unit = {
    def coords(ridx: Int, cidx: Int, painted: Int, count: Int): List[(Int, Int)] =
      (painted until (painted + count)).toList.map(idx => (cidx * 16 + (idx % 16)) -> (ridx * 16 + (idx / 16)))

    @tailrec
    def doPaint(lengths: List[Int], white: Boolean, painted: Int): Unit =
      lengths match {
        case h :: t =>
          gfx.setColor(if (white) Color.WHITE else Color.BLACK)
          coords(b.rowIdx, b.colIdx, painted, h).foreach({ case (x, y) =>
            gfx.fillRect(x, y, 1, 1)
          })
          doPaint(t, !white, painted + h)
        case Nil => if (painted != 256) sys.error("expected to paint full 256 pixels of a 16x16 block")
      }

    doPaint(ls, initialColorWhite, 0)
  }

  private def paintBlock(gfx: Graphics)(block: BlockWithCoordinates): Unit =
    block.block match {
      case WhiteBlock =>
        gfx.setColor(Color.WHITE)
        gfx.fillRect(block.colIdx * 16, block.rowIdx * 16, 16, 16)
      case BlackBlock =>
        gfx.setColor(Color.BLACK)
        gfx.fillRect(block.colIdx * 16, block.rowIdx * 16, 16, 16)
      case WhiteBlackBlock(ls) =>
        paintLengths(gfx, block, ls, initialColorWhite = true)

      case BlackWhiteBlock(ls) =>
        paintLengths(gfx, block, ls, initialColorWhite = false)
    }

  private def addLengths(
      blocksWithCoordinates: List[BlockWithCoordinates],
      input: InputStream
  ): List[BlockWithCoordinates] = {
    @tailrec
    def readBlockData(stream: InputStream, builder: MixedColorBlockBuilder, read: Int): Unit = {
      val length: Int = stream.read()
      if (length < 0) throw new RuntimeException("End of stream!")
      builder.addLength(length)
      if (read + length < 256) readBlockData(stream, builder, read + length)
    }

    blocksWithCoordinates.map({
      case b @ BlockWithCoordinates(WhiteBlock, _, _) => b
      case b @ BlockWithCoordinates(BlackBlock, _, _) => b
      case b @ BlockWithCoordinates(mcb: MixedColorBlock, _, _) =>
        val blockBuilder = new MixedColorBlockBuilder(mcb.firstColor)
        readBlockData(input, blockBuilder, 0)
        b.copy(block = blockBuilder.build())
    })
  }

  def main(args: Array[String]): Unit = {
    val input: InputStream =
      args.headOption.fold(this.getClass.getClassLoader.getResourceAsStream("image.1bp"))(new FileInputStream(_))

    val dim                = ByteBuffer.wrap(input.readNBytes(4)).getInt
    val blockW: Int        = dim >>> 16
    val blockH: Int        = dim & 0x00ff
    val imgW: Int          = blockW * 16
    val imgH: Int          = blockH * 16
    val img: BufferedImage = new BufferedImage(imgW, imgH, BufferedImage.TYPE_INT_RGB)
    val gfx: Graphics      = img.getGraphics
    gfx.setColor(Color.MAGENTA)
    gfx.fillRect(0, 0, imgW, imgH)

    val blockDescriptorBytes: Array[Byte] = input.readNBytes(Math.ceil((blockW * blockH).toDouble / 4).toInt)
    val blockDescriptors: List[Block]     = unpack(blockDescriptorBytes)
    val blocksWithCoordinates             = resolveCoordinates(blockDescriptors, blockW)
    val readyToPainBlocks                 = addLengths(blocksWithCoordinates, input)
    readyToPainBlocks.foreach(paintBlock(gfx))
    ImageIO.write(img, "PNG", new FileOutputStream("decodedImage.png"))
  }

}
