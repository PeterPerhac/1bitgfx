package com.perhac.utils.images.onebit

import better.files._
import com.perhac.utils.images.onebit.OneBitCodec.{cannotOverwriteExistingFile, time}
import com.perhac.utils.images.onebit.animation.AnimationConfig

import java.awt.image.BufferedImage
import java.awt.image.BufferedImage.TYPE_INT_RGB
import java.awt.{Color, Graphics}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, FileOutputStream, RandomAccessFile}
import java.nio.file.Paths
import java.util.zip.{Inflater, InflaterInputStream}
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object OneBitDecoder {

  def readMetadata(input: RandomAccessFile): OneBitPictureMetadata = {
    val ac     = AnimationConfig.fromByte(input.read())
    val blockW = input.read()
    val blockH = input.read()
    OneBitPictureMetadata(
      animationConfig = ac,
      blockW = blockW,
      blockH = blockH,
      blockDescriptorByteCount = Math.ceil((blockW * blockH).toDouble / 4).toInt
    )
  }

  def decodeImage(blockDescriptorBytes: Array[Byte], input: RandomAccessFile, blockW: Int)(gfx: Graphics): Unit = {
    val blockDescriptors: ArrayBuffer[Block] = unpack(blockDescriptorBytes)
    val blocksWithCoordinates                = resolveCoordinates(blockDescriptors, blockW)
    val readyToPaintBlocks                   = addLengths(blocksWithCoordinates, input)
    readyToPaintBlocks.foreach(paintBlock(gfx))
  }

  def decode(inPath: String, outPath: Option[String], overwriteExisting: Boolean): Unit = {
    val input: RandomAccessFile           = new RandomAccessFile(inPath, "r")
    val metadata                          = readMetadata(input)
    val img: BufferedImage                = new BufferedImage(metadata.imgW, metadata.imgH, TYPE_INT_RGB)
    val gfx: Graphics                     = img.getGraphics
    val blockDescriptorBytes: Array[Byte] = new Array[Byte](metadata.blockDescriptorByteCount)
    input.read(blockDescriptorBytes)
    decodeImage(blockDescriptorBytes, input, metadata.blockW)(gfx)
    gfx.dispose()

    val inFile = inPath.toFile
    val outFilePath =
      outPath.getOrElse(inFile.parent.path.resolve(inFile.nameWithoutExtension + ".png").toAbsolutePath.toString)

    if (!Paths.get(outFilePath).toFile.exists() || overwriteExisting) {
      val outStream = new FileOutputStream(outFilePath)
      try {
        time("write image to output") {
          ImageIO.write(img, "PNG", outStream)
          System.out.println("output file saved to: " + outFilePath)
        }
      } finally {
        outStream.close()
      }
    } else {
      cannotOverwriteExistingFile()
    }
  }

  def unpack(bytes: Array[Byte]): ArrayBuffer[Block] = {
    val buf: ArrayBuffer[Block] = new ArrayBuffer[Block](bytes.length * 4) //4 descriptors per byte
    bytes.foreach { b =>
      buf.addOne(Block.fromDescriptor(((b & 0xc0) >>> 6).toByte))
      buf.addOne(Block.fromDescriptor(((b & 0x30) >>> 4).toByte))
      buf.addOne(Block.fromDescriptor(((b & 0xc) >>> 2).toByte))
      buf.addOne(Block.fromDescriptor((b & 0x3).toByte))
    }
    buf
  }

  def resolveCoordinates(blockDescriptors: ArrayBuffer[Block], blockW: Int): ArrayBuffer[BlockWithCoordinates] = {
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
      ls: ArrayBuffer[Int],
      initialColorWhite: Boolean
  ): Unit = {
    def coords(ridx: Int, cidx: Int, painted: Int, count: Int): IndexedSeq[(Int, Int)] =
      (painted until (painted + count)).map(idx => (cidx * 16 + (idx % 16)) -> (ridx * 16 + (idx / 16)))

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

    doPaint(ls.toList, initialColorWhite, 0)
  }

  def paintBlock(gfx: Graphics)(block: BlockWithCoordinates): Unit =
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

  def addLengths(
      blocksWithCoordinates: ArrayBuffer[BlockWithCoordinates],
      input: RandomAccessFile
  ): ArrayBuffer[BlockWithCoordinates] = {
    @tailrec
    def readBlockData(stream: ByteArrayInputStream, builder: MixedColorBlockBuilder, read: Int): Unit = {
      val length: Int = stream.read()
      if (length < 0) throw new RuntimeException("End of stream!")
      builder.addLength(length)
      if (read + length < 256) readBlockData(stream, builder, read + length)
    }

    def decompressBlockData(compressedBytes: Array[Byte]): ByteArrayInputStream = {
      val currentFrameData: ByteArrayInputStream = new ByteArrayInputStream(compressedBytes)
      val inflaterStream                         = new InflaterInputStream(currentFrameData, new Inflater(true))
      val inflatedByteStream                     = new ByteArrayOutputStream()
      var read: Int                              = 0
      val buffer                                 = new Array[Byte](Math.max(compressedBytes.length / 4, 1024))
      try {
        do {
          read = inflaterStream.read(buffer)
          if (read > 0) inflatedByteStream.write(buffer, 0, read)
        } while (read > 0)
      } finally {
        inflaterStream.close()
      }

      new ByteArrayInputStream(inflatedByteStream.toByteArray)
    }

    val dataBlockSize                = input.readInt()
    val compressedBytes: Array[Byte] = new Array[Byte](dataBlockSize)
    input.readFully(compressedBytes)
    val blockData: ByteArrayInputStream = decompressBlockData(compressedBytes)

    try {
      blocksWithCoordinates.map {
        case b @ BlockWithCoordinates(mcb: MixedColorBlock, _, _) =>
          val blockBuilder = new MixedColorBlockBuilder(mcb.firstColor)
          readBlockData(blockData, blockBuilder, 0)
          b.copy(block = blockBuilder.build())
        case b => b
      }
    } finally {
      blockData.close()
    }
  }

}
