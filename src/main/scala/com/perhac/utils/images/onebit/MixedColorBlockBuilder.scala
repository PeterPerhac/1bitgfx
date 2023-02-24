package com.perhac.utils.images.onebit

import scala.collection.mutable.ArrayBuffer

class MixedColorBlockBuilder(firstLengthColor: BlockColor) {

  private val lengthBuffer: ArrayBuffer[Int] = new ArrayBuffer[Int](32)

  def addLength(l: Int): MixedColorBlockBuilder = {
    this.lengthBuffer.addOne(l)
    this
  }

  def build(): MixedColorBlock = this.firstLengthColor match {
    case Black => BlackWhiteBlock(lengthBuffer)
    case White => WhiteBlackBlock(lengthBuffer)
  }

}
