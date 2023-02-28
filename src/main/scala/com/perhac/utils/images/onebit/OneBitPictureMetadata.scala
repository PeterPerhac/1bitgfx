package com.perhac.utils.images.onebit

import com.perhac.utils.images.onebit.animation.AnimationConfig

case class OneBitPictureMetadata(
    animationConfig: AnimationConfig,
    blockW: Int,
    blockH: Int,
    blockDescriptorByteCount: Int
) {
  val isStillImage: Boolean = animationConfig.isStillImage
  val imgW: Int             = blockW * 16
  val imgH: Int             = blockH * 16
}
