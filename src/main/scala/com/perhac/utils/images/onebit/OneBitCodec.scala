package com.perhac.utils.images.onebit

import com.monovore.decline.Opts.{argument, flag}
import com.monovore.decline._
import cats.implicits._

object OneBitCodec {

  private val inFile = argument[String]("in")

  private val outFile =
    Opts
      .option[String](long = "out", help = "file path to the output file or directory", short = "o", metavar = "path")
      .orNone

  private val threshold = Opts
    .option[Float](
      long = "threshold",
      help =
        "Threshold beyond which a pixel will be considered white. In the range between 0 (all pixels will be considered white) and 3 (all pixels will be considered black)",
      short = "t",
      metavar = "threshold",
      visibility = Visibility.Partial
    )
    .withDefault(2.0f)

  private val encodeFlag = flag("encode", "encode the text in file specified", "e").map(_ => encode)
  private val decodeFlag = flag("decode", "decode the text in file specified", "d").map(_ => decode)
  private val operation  = encodeFlag orElse decodeFlag

  val main: Opts[Unit] = (inFile, operation, outFile, threshold).mapN { (in, operation, out, t) =>
    operation(in, out, t)
  }

  val encode: (String, Option[String], Float) => Unit = OneBitEncoder.encode
  val decode: (String, Option[String], Float) => Unit = (in, out, _) => OneBitDecoder.decode(in, out)

}

object OneBitCodecApp
    extends CommandApp(
      name = "one-bit-codec",
      header =
        "Utility for converting images to 1-bit images optimising for disk space, and for decoding the images back into a PNG file.",
      main = OneBitCodec.main,
      helpFlag = true,
      version = "0.1"
    )
