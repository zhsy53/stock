package com.cat.zsy.strategy

import com.cat.zsy.domain._
import com.cat.zsy.util.StockUtils._

import scala.collection.mutable.ListBuffer

object IndicatorsCalculator {
  def calc(seq: TongStockHistory, periods: Seq[Int]): TongStockCompoundIndicators = {
    TongStockCompoundIndicators(seq, periods.map(_calc(seq.data, _)).filter(_.enough))
  }

  def calc(seq: TongStockHistory, periods: Int, count: Int): TongStockCompoundIndicators = {
    val list = seq.data.reverse.grouped(periods).toSeq.take(count)

    TongStockCompoundIndicators(seq, list.map(o => _calc(o, periods)).filter(_.enough))
  }

  private def _calc(data: Seq[TongStockElement], period: Int): TongStockIndicators = {
    if (data.size != period) return TongStockIndicators(period)

    val closingList = data.map(_.closingPrice)

    val avg = closingList.sum / data.size

    TongStockIndicators(
      period,
      enough = true,
      avg.toDouble / 100,
      variance(closingList),
      data.last.closingPrice.toDouble / 100,
      closingList.max.toDouble / 100,
      closingList.min.toDouble / 100,
      _oscillation(closingList, avg),
      data.map(o => (o.highestPrice - o.lowestPrice) * 100.toDouble / o.openingPrice)
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
