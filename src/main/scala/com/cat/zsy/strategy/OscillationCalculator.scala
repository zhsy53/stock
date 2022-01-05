package com.cat.zsy.strategy

import com.cat.zsy.domain._

import scala.collection.mutable.ListBuffer

/**
 * 均值震荡计算器
 */
object OscillationCalculator {
  def calcList(seq: TongStockHistory, periods: Seq[Int]): TongStockIndicatorsList = {
    TongStockIndicatorsList(seq, periods.map(calc(seq, _)).filter(_.nonEmpty).map(_.get))
  }

  def calc(seq: TongStockHistory, period: Int): Option[TongStockIndicators] = {
    val data = seq.data

    val last = data.last

    if (last.date < 20211228 || data.size <= 40) return Option.empty

    val enough = data.size >= period

    val list = data.takeRight(period)

    val avg = list.map(_.closingPrice).sum / list.size

    val current = last.closingPrice

    val amplitude = list.map(o => (o.closingPrice - o.openingPrice) * 100 / o.openingPrice)

    val oscillation = mapToUpDown(list.map(_.closingPrice), avg)

    Option(TongStockIndicators(period, enough, avg, current, oscillation, amplitude))
  }

  private def mapToUpDown(list: Seq[Int], base: Int): Seq[Int] = {
    val r = ListBuffer[Int]()

    val filter = list.filter(_ != base)

    r += { if (filter.head > base) 1 else -1 }

    val boolToSign: Boolean => Int = o => if (o) 1 else -1

    filter.tail.foreach(o => if (o > base ^ r.last > 0) r += boolToSign(o > base) else r.update(r.size - 1, r.last + boolToSign(o > base)))

    r.toSeq
  }
}
