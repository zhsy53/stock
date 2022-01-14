package com.cat.zsy

import com.cat.zsy.api.{SinaQuotesApi, TongStockApi}
import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.strategy.{IndicatorsCalculator, TongStockCompoundIndicators}
import org.slf4j.LoggerFactory

/**
 * 关注
 * 300718;300461;300200;002050;002043;000063;300094;000888;002851;000938;000712;000927;002207；
 * 601919;600988;688981;
 *
 * 待定
 * 000878;002214;300117;300369;300719;600739;603186
 * 其中:300369;002214;
 */
object Application extends App {
  val log = LoggerFactory.getLogger(this.getClass)
//  val dir = "/Users/xiu/Downloads/stock"
  val dir = "D:\\stock"

  // config
  val listDuration = List(100, 200, 300, 400, 500, 600)
  val chooseDuration = List(100, 200, 300, 400, 500, 600)
  // 最高单价
  val maxPrice = 35
  // 最大低谷期
  val maxDownOscillation = 33
  // 平均振幅(%)
  val avgAmplitude = 3.5
  // 均值方差
  val avgVariance = 0.014
  // 最小盈利空间(%)
  val profitRatio = 4.0

  val begin = System.currentTimeMillis()

  val myCodes =
    "002370;300094;300461;300718;603212;002050;002043"
      .split(";")
      .distinct
      .sorted
      .toList

  choose()
//
//  choose(codes, table = true, detail = false)
  val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

  private def list(): Seq[String] = {
    TongStockApi
      .listAllDataFromFilepath(dir)
      .map(o => TongStockHistory(o.code, o.data.drop(22))) // 模拟一个月前预测
      .map(o => IndicatorsCalculator.calc(o, listDuration))
      .filter(o => o.indicators.count(_.enough) >= 4) // 排除统计区间不足的
      .filter(o => o.avgDownOscillation <= maxDownOscillation || o.maxDownOscillation <= maxDownOscillation * 1.5) // 低谷驻留时间不超过**
      .filter(_.avgAmplitude >= avgAmplitude) // 平均振幅->行情活跃
      .filter(o => o.avgVariance <= 0.02) // 均值方差->稳定
      .filter(o => o.indicators.head.avgPrice <= maxPrice) // 最高单价
      .filter(o => o.indicators.map(_.avgPrice).min >= o.indicators.head.closingPrice) // 盈利空间
      .map(_.stock.code.substring(2))
      .sorted
  }

  private def buy(list: Seq[TongStockCompoundIndicators], filter: Boolean = false, detail: Boolean = false, simulation: Boolean = false): Unit = {
    val codes = list.map(_.stock.code.substring(2))

    val currentMap = SinaQuotesApi.getData(codes).map(o => (o.code.substring(2), o)).toMap
    val historyMap = list.map(o => (o.stock.code.substring(2), o)).toMap

    codes.foreach(_show)

    def _show(code: String): Unit = {
      val tong = historyMap(code)
      val sina = currentMap(code)

      val except = tong.indicators.map(_.avgPrice).take(2).min

      val simulationPrice = tong.indicators.last.closingPrice
      val currentPrice = sina.currentPrice.toDouble
      val price = if (simulation) simulationPrice else currentPrice

      val pass = !sina.name.contains("ST") && except >= price

      if (!pass && filter) return

      if (detail) log.info(tong.indicators.map(_.toString).mkString("\n"))

      log.info(
        "{}\t{}\t{}\t模拟:{}\t现价:{}\t成功:{}\t预期价:{}\t预期盈利:{}\t{}",
        if (pass) "+++" else "---",
        code,
        f"${sina.name}%-7s",
        f"$simulationPrice%2.2f",
        f"$currentPrice%2.2f",
        s"${currentPrice >= simulationPrice}",
        f"$except%.2f",
        f"${(except - currentPrice) * 100 / currentPrice}%.2f${"%"}",
        tong.toString
      )
    }
  }

  private def choose(): Unit = {
    val month = 22
    val periods = month * 3
    val count = month * 12 * 2 / periods

    val avgFilter: TongStockCompoundIndicators => Boolean = o => {
      val prices = o.indicators.map(_.avgPrice)
      val increase = Range(1, prices.size).map(i => prices(i) / prices(i - 1))

      increase.filter(_ < 1).map(1 - _).forall(_ <= 0.11)
    }

    val list = TongStockApi
      .listAllDataFromFilepath(dir)
      .map(o => IndicatorsCalculator.calc(o, periods, count))
      .filter(o => o.indicators.count(_.enough) >= count) // 排除统计区间不足的
      .filter(o => o.indicators.head.avgPrice <= maxPrice) // 最高单价
      .filter(o => o.avgDownOscillation <= month * 1.1 || o.maxDownOscillation <= month * 1.5) // 低谷驻留时间 均值不超过** 或 最大值不超过***
      .filter(_.avgAmplitude >= avgAmplitude) // 平均振幅->行情活跃
      .filter(o => o.avgVariance <= avgVariance && avgFilter(o)) // 均值方差->稳定
      .sortBy(_.avgVariance)

    log.info("满足条件的有:{}只\n{}", list.size, list.map(_.stock.code.substring(2)).mkString(";"))

    buy(list, filter = true, simulation = true)
  }
}
