package com.cat.zsy.api
import com.cat.zsy.domain.{TongStockElement, TongStockHistory}
import com.cat.zsy.parser.TongStockElementParser

import java.io.File
import java.net.URI
import java.nio.file.{Files, Path, Paths}
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

object TongStockApi {
  private val fileSuffix = ".day"

  def getStockSeqByCodeFromDir(code: String, dir: String): Seq[TongStockElement] = TongStockElementParser.parse(Files.readAllBytes(Path.of(dir + File.separator + code + fileSuffix)))

  private def listAllData(uri: URI): Seq[TongStockHistory] = {
    def getCodeFromFilename(filename: String): String = filename.substring(0, filename.indexOf(fileSuffix))

    def listDataByFile(file: File): Seq[TongStockElement] = TongStockElementParser.parse(Files.readAllBytes(file.toPath))

    // 沪A
    val ha = Seq("sh600", "sh601", "sh603", "sh605")
    // 科创 (20%)
    val kc = Seq("sh688")
    // 深A
    val sa = Seq("sz000", "sz001", "sz002", "sz003")
    // 创业 (20%)
    val cy = Seq("sz300")

    val stocks = Seq.concat(cy, kc, ha, sa)

    Paths
      .get(uri)
      .toFile
      .listFiles
      .filter(_.isFile)
      .filter(_.getName.endsWith(".day"))
      .filter(f => stocks.contains(f.getName.substring(0, 5)))
      .par
      .map(f => TongStockHistory(getCodeFromFilename(f.getName), listDataByFile(f)))
      .filter(_.data.last.date >= 20220101)
      .toList
  }

  def listAllDataFromFilepath(path: String): Seq[TongStockHistory] = listAllData(Path.of(path).toUri)
}
