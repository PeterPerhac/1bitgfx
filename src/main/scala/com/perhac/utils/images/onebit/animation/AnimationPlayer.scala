package com.perhac.utils.images.onebit.animation

import com.perhac.utils.images.onebit.{Block, OneBitDecoder}
import org.bytedeco.javacv.CanvasFrame

import java.awt.Graphics
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import java.io.RandomAccessFile
import java.nio.file.Paths
import javax.swing.WindowConstants
import scala.collection.mutable.ArrayBuffer

object AnimationPlayer {

  private def scanForFrames(input: RandomAccessFile, blockDescriptorByteCount: Int): Array[Int] = {
    val buf: ArrayBuffer[Int] = new ArrayBuffer[Int](320)
    do {
      buf.addOne(input.getFilePointer.toInt)
      input.skipBytes(blockDescriptorByteCount)
      val dataSize: Int = input.readInt()
      input.skipBytes(dataSize)
    } while (input.getFilePointer < input.length())
    buf.toArray
  }

  val nextElementAndState: PlaybackState => Option[(Int, PlaybackState)] = s => {
    if (s.forward) {
      if ((s.currentFrame + 1) == s.offsets.length) {
        s.playbackMode match {
          case DontPlay => None
          case PlayOnce => None
          case Loop     => Some((s.offsets(0), s.copy(currentFrame = 0)))
          case Bounce   => Some((s.offsets(s.currentFrame), s.copy(currentFrame = s.currentFrame - 1, forward = false)))
        }
      } else {
        Some(s.offsets(s.currentFrame), s.copy(currentFrame = s.currentFrame + 1))
      }
    } else {
      if (s.currentFrame == 0) {
        s.playbackMode match {
          case DontPlay | PlayOnce | Loop =>
            System.err.println(s"oops. Shouldn't be playing backwards when in ${s.playbackMode} mode")
            sys.exit(5)
          case Bounce => Some((s.offsets(s.currentFrame), s.copy(currentFrame = s.currentFrame + 1, forward = true)))
        }
      } else {
        Some(s.offsets(s.currentFrame), s.copy(currentFrame = s.currentFrame - 1))
      }
    }
  }

  def play(inPath: String): Unit = {

    val input = new RandomAccessFile(Paths.get(inPath).toFile, "r")
    try {
      val animationConfig: AnimationConfig = AnimationConfig.fromByte(input.read()).copy(playbackMode = Bounce)
      if (animationConfig.isStillImage) {
        System.err.println("Attempting to play back a still image. Use the decode utility for decoding still images.")
        sys.exit(4)
      }
      val blockW: Int              = input.read()
      val blockH: Int              = input.read()
      val blockDescriptorByteCount = Math.ceil((blockW * blockH).toDouble / 4).toInt
      //the array will hold block descriptors for any frame, as there will always be the same number of blocks in each frame of an animation
      val blockDescriptorBytes: Array[Byte] = new Array[Byte](blockDescriptorByteCount)

      val frameOffsets: Array[Int] = scanForFrames(input, blockDescriptorByteCount)
      val imgW: Int                = blockW * 16
      val imgH: Int                = blockH * 16
      val img: BufferedImage       = new BufferedImage(imgW, imgH, BufferedImage.TYPE_INT_RGB)
      val gfx: Graphics            = img.getGraphics
      val canvas                   = new CanvasFrame("1 Bit Animation Player")
      canvas.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

      val initialState: PlaybackState      = PlaybackState(frameOffsets, playbackMode = animationConfig.playbackMode)
      val frameOffsetStream: LazyList[Int] = LazyList.unfold(initialState)(nextElementAndState)

      var startT: Long = 0L
      frameOffsetStream.foreach { offset =>
        Thread.sleep(Math.max(0L, (1000 / animationConfig.fps).toLong - (System.currentTimeMillis() - startT)))
        input.seek(offset)
        input.readFully(blockDescriptorBytes)
        val blockDescriptors: ArrayBuffer[Block] = OneBitDecoder.unpack(blockDescriptorBytes)
        val blocksWithCoordinates                = OneBitDecoder.resolveCoordinates(blockDescriptors, blockW)
        val readyToPaintBlocks                   = OneBitDecoder.addLengths(blocksWithCoordinates, input)
        readyToPaintBlocks.foreach(OneBitDecoder.paintBlock(gfx))
        canvas.showImage(img)
        startT = System.currentTimeMillis()
      }

      canvas.dispatchEvent(new WindowEvent(canvas, WindowEvent.WINDOW_CLOSING))

      gfx.dispose()
    } finally {
      input.close()
    }

  }

}
