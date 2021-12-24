package com.cat.zsy.strategy

import com.cat.zsy.domain._
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object StrategyFactory {
  private val log = LoggerFactory.getLogger(this.getClass)

  private def price(i: Int): BigDecimal = BigDecimal(i) / 100

  // 涨幅 * 100
  private def mapToIncrease(list: Seq[StockElement]): Seq[Int] = list.map(o => o.closingPrice * 100 / o.openingPrice - 100)

  /**
   * 涨跌惯性
   */
  def inertia(seq: StockSeq, days: Int = 40, radios: Seq[(Int, Int)] = List((0, 70), (3, 40))): Boolean = {
    if (seq.data.size <= days) { return false }

    val list = seq.data.takeRight(days)

    val increase = mapToIncrease(list)
    val pass = radios.map(o => increase.count(_ >= o._1) >= days * o._2 / 100).reduce((a, b) => a && b)

    if (pass) { log.info("{} 的涨幅记录:\n{}", seq.code, increase.mkString(",")) }

    pass
  }

  // 均值震荡策略
  def oscillation(seq: StockSeq, days: Int, frequency: Int = 40, detail: Boolean = false): Boolean = {
    if (seq.data.size <= days) {
      log.trace("{} => 数据太少,不支持待统计区间", seq.code)
      return false
    }

    val list = seq.data.takeRight(days)

    val last = list.last

    if (last.date < 20211221) {
      log.trace("**股 {} 已经退市", seq.code)
      return false
    }

    val current = last.closingPrice

    if (price(current) >= 50) {
      log.trace("{} 太贵了买不起", seq.code)
      return false
    }

    // 均值两侧的分布曲线
    val avg = list.map(_.closingPrice).sum / list.size

    val upDown = mapToUpDown(list.map(_.closingPrice), avg)

    val downMax = -upDown.filter(_ < 0).min

    // 活跃行情:振幅超过**
    val active = list.count(o => o.highestPrice - o.lowestPrice >= o.openingPrice * 0.02) >= list.size * 0.5

    val pass = downMax <= frequency && (current >= avg * 0.8) && active

    if (detail || pass) { log.debug("{} 当前:{}\t均值:{}\t分布曲线:{}", seq.code, price(current).setScale(2), price(avg).setScale(2), upDown.filter(o => Math.abs(o) > 2).mkString("[", ",", "]")) }

    pass
  }

  private def mapToUpDown(list: Seq[Int], base: Int): Seq[Int] = {
    val r = ListBuffer[Int]()

    val filter = list.filter(_ != base)

    r += { if (filter.head > base) 1 else -1 }

    val boolToSign: Boolean => Int = o => if (o) 1 else -1

    // filter.tail.foreach(o => {
    //  if (o > base) {
    //    if (r.last < 0) { r += 1 }
    //    else { r.update(r.size - 1, r.last + 1) }
    //  } else {
    //    if (r.last > 0) { r += -1 }
    //    else { r.update(r.size - 1, r.last - 1) }
    //  }
    // })

    filter.tail.foreach(o => { if (o > base ^ r.last > 0) r += boolToSign(o > base) else r.update(r.size - 1, r.last + boolToSign(o > base)) })

    r.toSeq
  }
}
