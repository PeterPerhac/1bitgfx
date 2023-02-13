package com.perhac.utils.images.onebit
import java.awt.Color

sealed trait Block {

  def descriptor: Byte

  override def toString: String = this match {
    case WhiteBlock => "white block"
    case BlackBlock => "black block"
    case block: MixedColorBlock =>
      block match {
        case WhiteBlackBlock(lengths) =>
          "white first block with lengths: " + lengths.mkString(",")
        case BlackWhiteBlock(lengths) =>
          "black first block with lengths: " + lengths.mkString(",")
      }
  }

}

case object WhiteBlock extends Block {
  override def descriptor: Byte = 0x03.toByte
}
case object BlackBlock extends Block {
  override def descriptor: Byte = 0x00.toByte
}

sealed trait MixedColorBlock extends Block {
  def lengths: List[Int]
}

case class WhiteBlackBlock(lengths: List[Int]) extends MixedColorBlock {
  override def descriptor: Byte = 0x02.toByte
}
case class BlackWhiteBlock(lengths: List[Int]) extends MixedColorBlock {
  override def descriptor: Byte = 0x01.toByte
}

sealed trait BlockColor
case object Black extends BlockColor
case object White extends BlockColor

object BlockColor {
  def fromAwtColor(c: Color): BlockColor =
    if (OneBitEncoder.isWhitePixel(c)) White else Black
}
