package com.perhac.utils.images.onebit

import java.awt.Image
import java.awt.image.BufferedImage

object ImageUtils {

  def resize(img: BufferedImage, newW: Int, newH: Int): BufferedImage = {
    val tmp  = img.getScaledInstance(newW, newH, Image.SCALE_SMOOTH)
    val dimg = new BufferedImage(newW, newH, BufferedImage.TYPE_INT_ARGB)
    val g2d  = dimg.createGraphics
    g2d.drawImage(tmp, 0, 0, null)
    g2d.dispose()
    dimg
  }

}
