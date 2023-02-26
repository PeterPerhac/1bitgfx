package com.perhac.utils.images.onebit

import better.files._
import cats.implicits._
import com.monovore.decline.Opts.{argument, flag}
import com.monovore.decline._
import com.perhac.utils.images.onebit.animation.AnimationPlayer

import scala.util.Try

case class Config(in: String, out: Option[String], threshold: Option[Float], force: Boolean)

object OneBitCodec {
  def cannotOverwriteExistingFile(): Unit = {
    System.err.println(
      "Cannot overwrite existing output file. If you insist on overwriting, run this program with the -f or --force argument."
    )
    sys.exit(1)
  }

  def time[R](name: String)(block: => R): R = {
    val t0     = System.nanoTime()
    val result = block // evaluate block
    val t1     = System.nanoTime()

    val ms = (t1 - t0).toDouble / 1000000
    println(f"Elapsed time of operation [$name]: $ms%.3f ms")

    result
  }

  private val inFile: Opts[String] = argument[String]("in")

  private val outFile =
    Opts
      .option[String](long = "out", help = "file path to the output file or directory", short = "o", metavar = "path")
      .orNone

  private val threshold = Opts
    .option[String](
      long = "threshold",
      help =
        "threshold (float) above which a pixel will be considered white. In the range between 0 (all pixels will be considered white) and 1 (all pixels will be considered black)",
      short = "t",
      metavar = "threshold",
      visibility = Visibility.Partial
    )
    .withDefault("")

  private val encodeFlag = flag("encode", "encode the input image file as 1bp (1 bit picture)", "e").map(_ => encode)
  private val decodeFlag = flag("decode", "decode the input 1 bit picture file into PNG format", "d").map(_ => decode)
  private val recordFlag = flag("record", "record from webcam into an animated 1bp file", "r").map(_ => record)
  private val screenRecordFlag =
    flag("screen-record", "record the entire screen into an animated 1bp file", "s").map(_ => screenRecord)
  private val playbackFlag = flag("playback", "play back recorded 1bp animation", "p").map(_ => playback)
  private val forceFlag    = flag("force", "force-write the output file, overwriting any existing files", "f").orFalse
  private val operation    = encodeFlag orElse decodeFlag orElse recordFlag orElse screenRecordFlag orElse playbackFlag

  private def illegalThreshold = {
    System.err.println("Illegal threshold argument. Provide a floating point number in the [0..1] range")
    sys.exit(2)
  }

  val main: Opts[Unit] = (inFile, operation, outFile, threshold, forceFlag).mapN { (in, operation, out, t, force) =>
    val parsedThreshold = Try(t.toFloat).toOption match {
      case Some(value) => if (value >= 0.0f && value <= 1.0f) Some(value) else illegalThreshold
      case None        => if (t.trim.isEmpty) None else illegalThreshold
    }

    operation(Config(in, out, parsedThreshold, force))
  }

  val record: Config => Unit = conf => {
    val outPath = conf.out.getOrElse("recording.1bp")
    OneBitEncoder.record(outPath, conf.threshold, conf.force, webcam = true)
  }
  val screenRecord: Config => Unit = conf => {
    val outPath = conf.out.getOrElse("screen-recording.1bp")
    OneBitEncoder.record(outPath, conf.threshold, conf.force, webcam = false)
  }

  val playback: Config => Unit = conf => {
    AnimationPlayer.play(conf.in)
  }

  val encode: Config => Unit = conf => {
    time("encode") {
      val inFile = conf.in.toFile
      val outPath =
        conf.out.getOrElse(inFile.parent.path.resolve(inFile.nameWithoutExtension + ".1bp").toAbsolutePath.toString)
      OneBitEncoder.encodeSingleFile(conf.in, outPath, conf.threshold, conf.force)
    }
  }

  val decode: Config => Unit = conf => time("decode") { OneBitDecoder.decode(conf.in, conf.out, conf.force) }
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
