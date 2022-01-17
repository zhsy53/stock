package com.cat.zsy.strategy

import com.cat.zsy.domain._
import com.cat.zsy.util.StockUtils.avg

import scala.collection.mutable.ListBuffer

object IndicatorsCalculator {
  def calc(seq: TongStockHistory, duration: StrategyDuration): TongStockCompoundIndicators = {
    TongStockCompoundIndicators(seq, duration, seq.data.reverse.grouped(duration.period).toSeq.take(duration.count).map(_.reverse).map(_calc(_, duration.period)).filter(_.enough))
  }

  private def _calc(data: Seq[TongStockElement], period: Int): TongStockIndicators = {
    if (data.size != period) return TongStockIndicators(period)

    val closingList = data.map(_.closingPrice)

    val mean = closingList.sum / data.size

    TongStockIndicators(
      period,
      enough = true,
      mean.toDouble / 100,
      closingList.max.toDouble / 100,
      closingList.min.toDouble / 100,
      _oscillation(closingList, mean),
      avg(data.reverse.map(o => (o.highestPrice - o.lowestPrice) * 100 / o.openingPrice))
    )
  }

  private def _oscillation(list: Seq[Int], base: Int): Seq[Int] = {
    val r = ListBuffer[Int]()

    val filter = list.filter(_ != base)

    r += { if (filter.head > base) 1 else -1 }

    val boolToSign: Boolean => Int = o => if (o) 1 else -1

    filter.tail.foreach(o => if (o > base ^ r.last > 0) r += boolToSign(o > base) else r.update(r.size - 1, r.last + boolToSign(o > base)))

    r.toSeq
  }
}
