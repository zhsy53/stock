package com.cat.zsy.api
import com.cat.zsy.domain.{TongStockElement, TongStockHistory}
import com.cat.zsy.parser.TongStockElementParser

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

object TongStockApi {
  private val fileSuffix = ".day"

  // 沪A
  private val ha = Seq("sh600", "sh601", "sh603", "sh605")
  // 科创 (20%)
  private val kc = Seq("sh688")
  // 深A
  private val sa = Seq("sz000", "sz001", "sz002", "sz003")
  // 创业 (20%)
  private val cy = Seq("sz300")

  private val stocks = Seq.concat(cy, kc, ha, sa)

  def getDataByCodeFromDir(code: String, dir: String): Seq[TongStockElement] = listDataByPath(Path.of(dir + File.separator + code + fileSuffix))

  def listDataByPath(path: Path): Seq[TongStockElement] = TongStockElementParser.parse(Files.readAllBytes(path))

  def listDataFromDir(dir: String): Seq[TongStockHistory] = {
    def getCodeFromFilename(filename: String): String = filename.substring(0, filename.indexOf(fileSuffix))

    Paths
      .get(Path.of(dir).toUri)
      .toFile
      .listFiles
      .par
      .filter(_.isFile)
      .filter(_.getName.endsWith(fileSuffix))
      .filter(f => stocks.contains(f.getName.substring(0, 5)))
      .map(f => TongStockHistory(getCodeFromFilename(f.getName), listDataByFile(f)))
      .filter(_.data.last.date >= 20220101)
      .toList
  }

  private def listDataByFile(file: File): Seq[TongStockElement] = listDataByPath(file.toPath)
}
