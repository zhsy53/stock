package com.cat.zsy

import com.cat.zsy.cache.DataFactory
import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.strategy.config.{PriceStrategyOption, StockSlice}
import com.cat.zsy.strategy.executor.{ChipsExecutor, PriceExecutor, PriceIndicatorFilter, TruncateDataHandler}
import com.cat.zsy.strategy.model.ChipsModel
import com.cat.zsy.util.MathUtils.month
import org.slf4j.LoggerFactory

object Application extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  private val begin = System.currentTimeMillis()

//  listBySliceAndSorted(StockSlice(month * 4)).foreach(done)

  test()

  private val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

  private def test(): Unit = {
    val r = DataFactory.data
      .filter(o => List("300718", "300117", "002043").contains(o.code.substring(2)))
      .filter(o => !DataFactory.getName(o.code.substring(2)).contains("ST"))
      .filter(o => o.data.last.closingPrice <= 40 * 100)
      .map(o => (o.code, ChipsExecutor.run(o)))

    def boolToSign: Boolean => Int = o => if (o) 1 else 0

    val cs = r.map { case (code, rs) =>
      val count = rs
        .map(o => (o.isProfit, o.isUnknown, o.isLoss))
        .map { case (b1, b2, b3) => (boolToSign(b1), boolToSign(b2), boolToSign(b3)) }
        .foldLeft((0, 0, 0))((pre, e) => (pre._1 + e._1, pre._2 + e._2, pre._3 + e._3))

      (code, rs, count)
    }

    cs.foreach(o => {
      log.info("{} {} => {}", o._1, f"${DataFactory.getName(o._1.substring(2))}%10s", o._3)

      o._2.map(_.toString).foreach(log.info)
    })

    val total = cs.map(_._3).foldLeft((0, 0, 0))((pre, e) => (pre._1 + e._1, pre._2 + e._2, pre._3 + e._3))
    log.warn(total.toString())
  }

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
