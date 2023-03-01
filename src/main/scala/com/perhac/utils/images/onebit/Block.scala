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

sealed trait BlockColor
case object Black extends BlockColor
case object White extends BlockColor

object BlockColor {

  case class PixelValue(value: Double)  extends AnyVal
  case class Midpoint(midpoint: Double) extends AnyVal
  type ClassifierFunction = PixelValue => Midpoint => BlockColor

  object DefaultClassifier extends ClassifierFunction {
    override def apply(pixelValue: PixelValue): Midpoint => BlockColor = mp => {
      if (pixelValue.value > (3 * mp.midpoint)) White else Black
    }
  }

  object ContouredClassifier extends ClassifierFunction {
    override def apply(pixelValue: PixelValue): Midpoint => BlockColor = mp => {
      if (pixelValue.value < mp.midpoint + 0.25 && pixelValue.value > mp.midpoint - 0.25) Black else White
    }
  }

  object LowAndHigh extends ClassifierFunction {
    override def apply(pixelValue: PixelValue): Midpoint => BlockColor = _ => {
      if (pixelValue.value < 0.75 || pixelValue.value > 2.25) White else Black
    }
  }

  def fromAwtColor(color: Color, midPoint: Float, classifier: ClassifierFunction): BlockColor =
    classifier(PixelValue(color.getRGBColorComponents(null).sum))(Midpoint(midPoint))

}

case class BlockWithCoordinates(block: Block, colIdx: Int, rowIdx: Int) {
  override def toString: String = s"${block.toString} at [$colIdx, $rowIdx]"
}
