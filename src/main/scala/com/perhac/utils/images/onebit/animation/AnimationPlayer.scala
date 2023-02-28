package com.perhac.utils.images.onebit.animation

import com.perhac.utils.images.onebit.OneBitDecoder
import org.bytedeco.javacv.CanvasFrame

import java.awt.Graphics
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import java.awt.image.BufferedImage.TYPE_INT_RGB
import java.io.RandomAccessFile
import java.nio.file.Paths
import javax.swing.WindowConstants
import scala.collection.mutable.ArrayBuffer

object AnimationPlayer {

  case class Frame(offset: Int, number: Int)

  private def findFrameOffsets(input: RandomAccessFile, blockDescriptorByteCount: Int): Array[Int] = {
    //rough guess at an average animation played at 32 fps for 10s
    val buf: ArrayBuffer[Int] = new ArrayBuffer[Int](320)
    do {
      buf.addOne(input.getFilePointer.toInt)
      input.skipBytes(blockDescriptorByteCount)
      val dataSize: Int = input.readInt()
      input.skipBytes(dataSize)
    } while (input.getFilePointer < input.length())
    buf.toArray
  }

  private val nextFrameSelector: PlaybackState => Option[(Frame, PlaybackState)] = s => {
    def nextState(forward: Boolean, frameNumberIncrement: Int => Int): Option[(Frame, PlaybackState)] =
      Some(
        Frame(s.offsets(s.currentFrame), s.currentFrame),
        s.copy(forward = forward, currentFrame = frameNumberIncrement(s.currentFrame))
      )

    if (s.forward) {
      if ((s.currentFrame + 1) == s.offsets.length) {
        s.playbackMode match {
          case DontPlay => None
          case PlayOnce => None
          case Loop     => Some((Frame(s.offsets(0), 0), s.copy(currentFrame = 0)))
          case Bounce   => nextState(forward = false, _ - 1)
        }
      } else {
        nextState(forward = true, _ + 1)
      }
    } else {
      //playing in backwards direction, don't need to check for playback mode, as it must be Bounce
      if (s.currentFrame == 0) nextState(forward = true, _ + 1) else nextState(forward = false, _ - 1)
    }
  }

  def play(inPath: String): Unit = {

    val input = new RandomAccessFile(Paths.get(inPath).toFile, "r")
    try {
      val metadata = OneBitDecoder.readMetadata(input)
      if (metadata.isStillImage) {
        System.err.println("Attempting to play back a still image. Use the decode utility for decoding still images.")
        sys.exit(4)
      }
      //the array will hold block descriptors for any frame, as there will always be the same number of blocks in each frame of an animation
      val blockDescriptorBytes: Array[Byte] = new Array[Byte](metadata.blockDescriptorByteCount)

      val frameOffsets: Array[Int] = findFrameOffsets(input, metadata.blockDescriptorByteCount)
      val img: BufferedImage       = new BufferedImage(metadata.imgW, metadata.imgH, TYPE_INT_RGB)
      val gfx: Graphics            = img.getGraphics

      val canvas = new CanvasFrame("1 Bit Animation Player")
      canvas.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

      val frameOffsetStream: LazyList[Frame] =
        LazyList.unfold(PlaybackState(frameOffsets, metadata.animationConfig.playbackMode))(nextFrameSelector)

      var startT: Long = 0L
      frameOffsetStream.foreach { frame =>
        canvas.setTitle(f"FPS: ${metadata.animationConfig.fps}%d, frame # ${frame.number}%d of ${frameOffsets.length}")
        Thread.sleep(Math.max(0L, (1000 / metadata.animationConfig.fps).toLong - (System.currentTimeMillis() - startT)))
        input.seek(frame.offset)
        input.read(blockDescriptorBytes)
        OneBitDecoder.decodeImage(blockDescriptorBytes, input, metadata.blockW)(gfx)
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
