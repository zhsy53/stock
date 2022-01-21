package com.cat.zsy.cache
import com.cat.zsy.api.{MetadataApi, TongStockApi}
import com.cat.zsy.domain.{MetadataElement, TongStockHistory}

object DataFactory {
  private val dirs = ("D:\\stock", "/Users/xiu/Downloads/stock")

  private def dir = if (System.getProperty("os.name").toLowerCase().contains("window")) dirs._1 else dirs._2

  val data: Seq[TongStockHistory] = TongStockApi.listAllDataFromFilepath(dir)
  val dicts: Seq[MetadataElement] = MetadataApi.getData

  private val dictMap = dicts.map(o => (o.code, o.name)).toMap
  def getName(code: String): String = dictMap.getOrElse(code, "")
}
