package com.perhac.utils.images.onebit.animation

import com.perhac.utils.images.onebit.{Block, OneBitDecoder}
import org.bytedeco.javacv.CanvasFrame

import java.awt.Graphics
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import java.io.{DataInputStream, FileInputStream}
import java.nio.file.Paths
import javax.swing.WindowConstants
import scala.collection.mutable.ArrayBuffer

object AnimationPlayer {

  def play(inPath: String): Unit = {

    val input = new DataInputStream(new FileInputStream(Paths.get(inPath).toFile))
    try {
      val animationConfig: AnimationConfig = AnimationConfig.fromByte(input.read().toByte)
      val blockW: Int                      = input.read()
      val blockH: Int                      = input.read()
      val imgW: Int                        = blockW * 16
      val imgH: Int                        = blockH * 16
      val img: BufferedImage               = new BufferedImage(imgW, imgH, BufferedImage.TYPE_INT_RGB)
      val gfx: Graphics                    = img.getGraphics

      val canvas = new CanvasFrame("1 Bit Animation Player")
      canvas.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

      var startT: Long = 0L
      do {
        Thread.sleep(Math.max(0L, (1000 / animationConfig.fps).toLong - (System.currentTimeMillis() - startT)))
        val blockDescriptorBytes: Array[Byte]    = input.readNBytes(Math.ceil((blockW * blockH).toDouble / 4).toInt)
        val blockDescriptors: ArrayBuffer[Block] = OneBitDecoder.unpack(blockDescriptorBytes)
        val blocksWithCoordinates                = OneBitDecoder.resolveCoordinates(blockDescriptors, blockW)
        val readyToPaintBlocks                   = OneBitDecoder.addLengths(blocksWithCoordinates, input)
        readyToPaintBlocks.foreach(OneBitDecoder.paintBlock(gfx))
        canvas.showImage(img)
        startT = System.currentTimeMillis()
      } while (input.available() > 0)

      canvas.dispatchEvent(new WindowEvent(canvas, WindowEvent.WINDOW_CLOSING))

      gfx.dispose()
    } finally {
      input.close()
    }

  }

}
