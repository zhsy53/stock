package com.cat.zsy

import com.cat.zsy.cache.DataFactory
import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.strategy.ChipsModel
import com.cat.zsy.strategy.config.{PriceStrategyOption, StockSlice}
import com.cat.zsy.strategy.executor.{PriceExecutor, PriceIndicatorFilter, TruncateDataHandler}
import com.cat.zsy.util.MathUtils.month
import org.slf4j.LoggerFactory

object Application extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  private val begin = System.currentTimeMillis()

//  listBySliceAndSorted(StockSlice(month * 4)).foreach(done)

  private val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

  private def showIncrease(): Unit = {
    listBySliceAndSorted(StockSlice(month * 4))
      .map(o => {
        val code = o.code
        val min = o.data.minBy(_.lowestPrice)
        val max = o.data.maxBy(_.highestPrice)
        val avg = o.data.map(_.closingPrice).sum / o.data.size
        val name = DataFactory.getName(code.substring(2))
        ChipsModel(code, name, avg, min.date, min.lowestPrice, max.date, max.highestPrice)
      })
      .filter(o => !o.isST && o.avg <= 35 * 100 && o.increase >= 30)
      .sortBy(-_.increase)
      .foreach(o => log.info(o.toString()))
  }

  private def listBySliceAndSorted(slice: StockSlice): Seq[TongStockHistory] = {
    val h = new TruncateDataHandler(slice)
    val f = PriceIndicatorFilter(PriceStrategyOption(maxPrice = Option(35), count = Option(slice.length)))

    PriceExecutor.handleByElementsAndFilter(h = Option(h), f = Option(f))
  }
}
