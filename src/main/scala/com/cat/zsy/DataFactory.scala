package com.cat.zsy

import com.cat.zsy.domain.{StockElement, StockSeq}
import com.cat.zsy.parser.StockElementParser

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path, Paths}
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

//https://www.tdx.com.cn/article/alldata.html
object DataFactory {
  val fileSuffix = ".day"

  def getStockSeqByCodeFromClasspath(code: String): Seq[StockElement] = StockElementParser.parseDates(Files.readAllBytes(Paths.get(getFileFromClasspath(code + fileSuffix))))

  private def getFileFromClasspath(filename: String): URI = this.getClass.getResource("/" + filename).toURI

  def getStockSeqByCodeFromDir(code: String, dir: String): Seq[StockElement] = StockElementParser.parseDates(Files.readAllBytes(Path.of(dir + File.separator + code + fileSuffix)))

  def listAllDataFromClasspath: Seq[StockSeq] = listAllData(getDirFromClasspath)

  private def getDirFromClasspath: URI = this.getClass.getResource("/").toURI

  private def listAllData(uri: URI): Seq[StockSeq] = {
    def getCodeFromFilename(filename: String): String = filename.substring(0, filename.indexOf(fileSuffix))

    def listDataByFile(file: File): Seq[StockElement] = StockElementParser.parseDates(Files.readAllBytes(file.toPath))

    // 创业 (20%)
    val cy = Seq("sz300")
    // 沪A
    val ha = Seq("sh600", "sh601", "sh603", "sh605")
    // 科创 (20%)
    val kc = Seq("sh688")
    // 深A
    val sa = Seq("sz000", "sz001", "sz002", "sz003")

    val all = Seq.concat(cy, kc, ha, sa)

    Paths.get(uri).toFile.listFiles.filter(_.isFile).filter(_.getName.endsWith(".day")).filter(f => all.contains(f.getName.substring(0, 5))).par
      .map(f => StockSeq(getCodeFromFilename(f.getName), listDataByFile(f))).toList
  }

  def listAllDataFromFilepath(path: String): Seq[StockSeq] = listAllData(Path.of(path).toUri)
}
