package com.cat.zsy.strategy.executor

import com.cat.zsy.domain.{TongStockElement, TongStockHistory}
import com.cat.zsy.strategy.config.StockSlice

trait DataHandler[T] {
  def handle(t: T): T
}

object DataHandler {
  def compose[T](hs: Seq[DataHandler[T]]): DataHandler[T] = t => hs.foldLeft(t)((pre, h) => h.handle(pre))
}

class TruncateDataHandler(slice: StockSlice) extends DataHandler[Seq[TongStockElement]] {
  override def handle(t: Seq[TongStockElement]): Seq[TongStockElement] = t.takeRight(slice.offset + slice.length).take(slice.length)
}

object TruncateDataHandler {
  def mapper(h: DataHandler[Seq[TongStockElement]]): DataHandler[TongStockHistory] = t => TongStockHistory(t.code, h.handle(t.data))
}
