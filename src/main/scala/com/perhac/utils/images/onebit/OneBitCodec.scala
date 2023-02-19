package com.perhac.utils.images.onebit

import better.files._
import cats.implicits._
import com.monovore.decline.Opts.{argument, flag}
import com.monovore.decline._

import scala.util.Try

object OneBitCodec {

  def time[R](name: Option[String] = None)(block: => R): R = {
    val t0     = System.nanoTime()
    val result = block // call-by-name
    val t1     = System.nanoTime()

    val inject = name.fold("")(n => s" of operation [$n]")
    println(s"Elapsed time$inject: " + (t1 - t0).toDouble / 1000000 + "ms")

    result
  }

  private val inFile = argument[String]("in")

  private val outFile =
    Opts
      .option[String](long = "out", help = "file path to the output file or directory", short = "o", metavar = "path")
      .orNone

  private val threshold = Opts
    .option[String](
      long = "threshold",
      help =
        "threshold (float|'auto') beyond which a pixel will be considered white. In the range between 0 (all pixels will be considered white) and 1 (all pixels will be considered black)",
      short = "t",
      metavar = "threshold",
      visibility = Visibility.Partial
    )
    .withDefault("0.7")

  private val encodeFlag = flag("encode", "encode the text in file specified", "e").map(_ => encode)
  private val decodeFlag = flag("decode", "decode the text in file specified", "d").map(_ => decode)
  private val operation  = encodeFlag orElse decodeFlag
  private def illegalThreshold = sys.error(
    "Illegal threshold argument. Provide a floating point number in the range 0..1 or use the 'auto' argument"
  )

  val main: Opts[Unit] = (inFile, operation, outFile, threshold).mapN { (in, operation, out, t) =>
    val parsedThreshold = Try(t.toFloat).toOption match {
      case Some(value) => if (value >= 0.0f && value <= 1.0f) Some(value) else illegalThreshold
      case None        => if (t.equalsIgnoreCase("auto")) None else illegalThreshold
    }
    operation(in, out, parsedThreshold)
  }

  val encode: (String, Option[String], Option[Float]) => Unit = (in, out, threshold) => {
    time(Some("encode")) {
      val inFile  = in.toFile
      val outPath = out.getOrElse(inFile.parent.path.resolve(inFile.nameWithoutExtension) + ".1bp")
      OneBitEncoder.encode(in, outPath, threshold)
    }
  }

  val decode: (String, Option[String], Option[Float]) => Unit = (in, out, _) =>
    time(Some("decode")) {
      OneBitDecoder.decode(in, out)
    }
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
