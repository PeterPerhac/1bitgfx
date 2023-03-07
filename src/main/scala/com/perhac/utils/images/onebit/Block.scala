package com.perhac.utils.images.onebit

import java.awt.Color
import scala.collection.mutable.ArrayBuffer

sealed trait Block {
  def descriptor: Byte
}

object Block {
  def fromDescriptor(desc: Byte): Block =
    desc match {
      case 0x03 => WhiteBlock
      case 0x00 => BlackBlock
      case 0x02 => WhiteBlackBlock(ArrayBuffer.empty)
      case 0x01 => BlackWhiteBlock(ArrayBuffer.empty)
    }
}

case object WhiteBlock extends Block {
  override val descriptor: Byte = 0x03.toByte
}
case object BlackBlock extends Block {
  override val descriptor: Byte = 0x00.toByte
}

sealed trait MixedColorBlock extends Block {
  def lengths: ArrayBuffer[Int]
  def firstColor: BlockColor
}

case class WhiteBlackBlock(lengths: ArrayBuffer[Int]) extends MixedColorBlock {
  override val descriptor: Byte       = 0x02.toByte
  override val firstColor: BlockColor = White
}
case class BlackWhiteBlock(lengths: ArrayBuffer[Int]) extends MixedColorBlock {
  override val descriptor: Byte       = 0x01.toByte
  override val firstColor: BlockColor = Black
}

sealed trait BlockColor {
  def getRGB: Int = this match {
    case Black => Color.BLACK.getRGB
    case White => Color.WHITE.getRGB
  }
}

case object Black extends BlockColor
case object White extends BlockColor

object BlockColor {

  case class PixelValue(value: Double)  extends AnyVal
  case class Midpoint(midpoint: Double) extends AnyVal

  sealed trait PixelClassifier {
    def classifyPixel(pixelValue: PixelValue, midpoint: Midpoint): BlockColor

  }

  object DefaultClassifier extends PixelClassifier {
    override def classifyPixel(pixelValue: PixelValue, mp: Midpoint): BlockColor = {
      if (pixelValue.value > (3 * mp.midpoint)) White else Black
    }

    override def toString: String = "DefaultClassifier (mid point is cut-off)"

  }

  object ContouredClassifier extends PixelClassifier {
    override def classifyPixel(pixelValue: PixelValue, mp: Midpoint): BlockColor = {
      if (pixelValue.value < mp.midpoint + 0.95 && pixelValue.value > mp.midpoint - 0.95) Black else White
    }

    override def toString: String = "ContouredClassifier (values around mid-point are black)"

  }

  object LowAndHigh extends PixelClassifier {
    override def classifyPixel(pixelValue: PixelValue, mp: Midpoint): BlockColor = {
      if (pixelValue.value < 0.75 || pixelValue.value > 2.25) White else Black
    }

    override def toString: String =
      "LowAndHigh (mid point is ignored, only very dark and bright areas are white, rest is black)"

  }

  def fromAwtColor(color: Color, midPoint: Float, classifier: PixelClassifier): BlockColor =
    classifier.classifyPixel(PixelValue(color.getRGBColorComponents(null).sum), Midpoint(midPoint))

}

case class BlockWithCoordinates(block: Block, colIdx: Int, rowIdx: Int) {
  override def toString: String = s"${block.toString} at [$colIdx, $rowIdx]"
}
