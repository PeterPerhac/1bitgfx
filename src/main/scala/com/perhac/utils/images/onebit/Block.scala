package com.perhac.utils.images.onebit

import java.awt.Color

sealed trait Block {
  def descriptor: Byte
}

object Block {
  def fromDescriptor(desc: Byte): Block =
    desc match {
      case 0x03 => WhiteBlock
      case 0x00 => BlackBlock
      case 0x02 => WhiteBlackBlock(List.empty)
      case 0x01 => BlackWhiteBlock(List.empty)
    }
}

case object WhiteBlock extends Block {
  override val descriptor: Byte = 0x03.toByte
}
case object BlackBlock extends Block {
  override val descriptor: Byte = 0x00.toByte
}

sealed trait MixedColorBlock extends Block {
  def lengths: List[Int]
  def firstColor: BlockColor
}

case class WhiteBlackBlock(lengths: List[Int]) extends MixedColorBlock {
  override val descriptor: Byte       = 0x02.toByte
  override val firstColor: BlockColor = White
}
case class BlackWhiteBlock(lengths: List[Int]) extends MixedColorBlock {
  override val descriptor: Byte       = 0x01.toByte
  override val firstColor: BlockColor = Black
}

sealed trait BlockColor
case object Black extends BlockColor
case object White extends BlockColor

object BlockColor {
  def fromAwtColor(color: Color, midPoint: Float): BlockColor = {
    // let AWT allocate the returned array by passing null
    if (color.getRGBColorComponents(null).sum > (3 * midPoint)) White else Black
  }
}

case class BlockWithCoordinates(block: Block, colIdx: Int, rowIdx: Int) {
  override def toString: String = s"${block.toString} at [$colIdx, $rowIdx]"
}
