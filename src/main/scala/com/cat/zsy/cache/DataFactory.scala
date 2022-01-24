package com.cat.zsy.cache
import com.cat.zsy.api.{MetadataApi, TongStockApi}
import com.cat.zsy.domain.{MetadataElement, TongStockHistory}

object DataFactory {
  private val dirs = ("D:\\stock", "/Users/xiu/Downloads/stock")
  private val dictMap = dicts.map(o => (o.code, o.name)).toMap

  def data: Seq[TongStockHistory] = TongStockApi.listDataFromDir(if (windows) dirs._1 else dirs._2)

  def windows: Boolean = System.getProperty("os.name").toLowerCase().contains("window")

  def dicts: Seq[MetadataElement] = MetadataApi.getData

  def getName(code: String): String = dictMap.getOrElse(code, "")
}
