package com.cat.zsy.util
import java.io.{File, InputStream}
import java.net.URI
import java.nio.file.Paths
import scala.io.{BufferedSource, Source}

object FileUtils {
  def getFileFromClasspath(filename: String): File = Paths.get(getUriFromClasspath(filename)).toFile

  def getUriFromClasspath(filename: String): URI = this.getClass.getResource("/" + filename).toURI

  def getInputStreamFromClasspath(filename: String): InputStream = getClass.getResourceAsStream("/" + filename)

  def getBufferedSourceFromClasspath(filename: String): BufferedSource = Source.fromResource(filename)

  def isToBytes(is: InputStream): Array[Byte] = LazyList.continually(is.read).takeWhile(_ != -1).map(_.toByte).toArray
}
