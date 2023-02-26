package com.perhac.utils.images.onebit.animation

case class PlaybackState(
    offsets: Array[Int],
    currentFrame: Int = 0,
    forward: Boolean = true,
    playbackMode: PlaybackMode = PlayOnce
)
