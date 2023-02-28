package com.perhac.utils.images.onebit.animation

case class PlaybackState(
    offsets: Array[Int],
    playbackMode: PlaybackMode = PlayOnce,
    currentFrame: Int = 0,
    forward: Boolean = true
)
