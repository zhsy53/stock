package com.cat.zsy.strategy

import com.cat.zsy.domain._
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object StrategyFactory {
  private val log = LoggerFactory.getLogger(this.getClass)

  def intToDecimal(i: Int): BigDecimal = (BigDecimal(i) / 100).setScale(2)

  // 涨幅 * 100
  private def mapToIncrease(list: Seq[StockElement]): Seq[Int] = list.map(o => o.closingPrice * 100 / o.openingPrice - 100)

  /**
   * 涨跌惯性
   */
  def inertia(seq: StockSeq, days: Int = 40, radios: Seq[(Int, Int)] = List((0, 70), (3, 40))): Boolean = {
    if (seq.data.size <= days) {
      return false
    }

    val list = seq.data.takeRight(days)

    val increase = mapToIncrease(list)
    val pass = radios.map(o => increase.count(_ >= o._1) >= days * o._2 / 100).reduce((a, b) => a && b)

    if (pass) {
      log.info("{} 的涨幅记录:\n{}", seq.code, increase.mkString(","))
    }

    pass
  }

  /**
   * @param purchase 购入价格
   * @param increase 期望涨幅
   * @param frequency 等待天数
   */
  def now(seq: StockSeq, days: Int, purchase: Option[Int], increase: Int, frequency: Int, detail: Boolean = false): Boolean = {
    val list = seq.data

    if (list.size <= days) return false

    val last = list.last

    if (last.date < 20211224) return false

    val base = purchase.getOrElse(last.closingPrice) * (100 + increase) / 100

    val upDown = mapToUpDown(list.map(_.closingPrice), base)

    val downMax = upDown.filter(_ < 0).map(-_).max

    val pass = downMax <= frequency

    if (pass || detail) {
      log.debug("{} {} -> {} 需{}天", seq.code, intToDecimal(last.closingPrice), intToDecimal(base), downMax)
    }

    pass
  }

  // 均值震荡策略
  def oscillations(seq: StockSeq, periods: Seq[Int]): StockStrategicIndicators = {
    StockStrategicIndicators(seq, periods.map(oscillation(seq, _)).filter(_.nonEmpty).map(_.get))
  }

  private def oscillation(seq: StockSeq, period: Int): Option[StrategicIndicators] = {
    val data = seq.data

    val last = data.last

    if (last.date < 20211228 || data.size <= 40) return Option.empty

    val enough = data.size >= period

    val list = data.takeRight(period)

    val avg = list.map(_.closingPrice).sum / list.size

    val current = last.closingPrice

    val amplitude = list.map(o => (o.closingPrice - o.openingPrice) * 100 / o.openingPrice)

    val oscillation = mapToUpDown(list.map(_.closingPrice), avg)

    Option(StrategicIndicators(period, enough, avg, current, oscillation, amplitude))
  }

  def oscillation(seq: StockSeq, days: Int, frequency: Int = 12, detail: Boolean = false): Boolean = {
    if (seq.data.size <= days) {
      log.trace("{} => 数据太少,不支持待统计区间", seq.code)
      return false
    }

    val list = seq.data.takeRight(days)

    val last = list.last

    if (last.date < 20211224) {
      log.trace("**股 {} 已经退市?", seq.code)
      return false
    }

    val current = last.closingPrice

    if (intToDecimal(current) >= 60) {
      log.trace("{} 太贵了买不起!", seq.code)
      return false
    }

    // 活跃行情:振幅或者波动超过**
    val active = list.count(o => o.highestPrice - o.lowestPrice >= o.openingPrice * 0.025) >= list.size * 0.4 ||
      list.count(o => o.closingPrice - o.openingPrice >= o.openingPrice * 0.015) >= list.size * 0.4

    // 均值
    val avg = list.map(_.closingPrice).sum / list.size

    // 当前是否直接入场
    val immediate = current >= avg * 0.85 && current < avg * 0.95

    // 均值震荡曲线
    val upDown = mapToUpDown(list.map(_.closingPrice), avg)

    val down = upDown.filter(_ < 0)
    if (down.isEmpty) {
      log.debug("{} 历史高点这么强的吗?", seq.code)
      return false
    }
    val downMax = -down.min

    // 低迷期
    val need = if (upDown.last > 0) -1 else if (downMax + upDown.last == 0) -1 else downMax + upDown.last

    val pass = active && immediate && downMax <= frequency // downMax <= frequency * 1.5 && need > 0 && need <= frequency)

    if (detail) {
      log.info(
        "{} (统计周期{}天)\t当前:{}\t均值:{}\t预期还需低迷:{}-{}={}\t活跃:{}\t当前适合:{}\t分布曲线:{}",
        seq.code,
        days,
        intToDecimal(current),
        intToDecimal(avg),
        downMax,
        -down.last,
        need,
        active,
        immediate,
        upDown.filter(o => Math.abs(o) > 2).mkString("[", ",", "]")
      )
    }

    downMax <= frequency
  }

  private def mapToUpDown(list: Seq[Int], base: Int): Seq[Int] = {
    val r = ListBuffer[Int]()

    val filter = list.filter(_ != base)

    r += { if (filter.head > base) 1 else -1 }

    val boolToSign: Boolean => Int = o => if (o) 1 else -1

    filter.tail.foreach(o => {
      if (o > base ^ r.last > 0) r += boolToSign(o > base) else r.update(r.size - 1, r.last + boolToSign(o > base))
    })

    r.toSeq
  }
}
