package com.perhac.utils.images.onebit.animation

sealed trait PlaybackMode {
  def descriptor: Byte
  def atFps(fps: Int): Byte = this match {
    case DontPlay => (fps << 2).toByte
    case PlayOnce => (fps << 2 | PlayOnce.descriptor).toByte
    case Loop     => (fps << 2 | Loop.descriptor).toByte
    case Bounce   => (fps << 2 | Bounce.descriptor).toByte
  }
}
case object DontPlay extends PlaybackMode {
  override def descriptor: Byte = 0x00
}
case object PlayOnce extends PlaybackMode {
  override def descriptor: Byte = 0x01
}
case object Loop extends PlaybackMode {
  override def descriptor: Byte = 0x02
}
case object Bounce extends PlaybackMode {
  override def descriptor: Byte = 0x03
}

case class AnimationConfig(fps: Byte, playbackMode: PlaybackMode) {
  val isStillImage: Boolean = fps == 0 && playbackMode == DontPlay

  override def toString: String = s"Animation config: FPS = $fps and playbackMode = $playbackMode"
}

object AnimationConfig {

  //the first byte in the file, when read as an unsigned byte - therefore an Int
  def fromByte(animationByte: Int): AnimationConfig = {
    AnimationConfig(
      ((animationByte & 0xfc) >>> 2).toByte,
      (animationByte & 0x03).toByte match {
        case 0x00 => DontPlay
        case 0x01 => PlayOnce
        case 0x02 => Loop
        case 0x03 => Bounce
      }
    )
  }

}
