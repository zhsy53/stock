package com.cat.zsy

import com.cat.zsy.api.{SinaQuotesApi, TongStockApi}
import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.strategy.{IndicatorsCalculator, StrategyOption, TongStockCompoundIndicators}
import com.cat.zsy.util.StockUtils.percentageToDecimal
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
  val dir = "/Users/xiu/Downloads/stock"
//  val dir = "D:\\stock"

  val option = StrategyOption.default

  val begin = System.currentTimeMillis()

  val allData = TongStockApi.listAllDataFromFilepath(dir)

  val myCodes =
    "002370;300094;300461;300718;603212;002050;002043"
      .split(";")
      .distinct
      .sorted
      .toList

  choose()

  log.info("---------------")

//  choose(33)

  val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

  /**
   * @param simulation 回退天数 -> 模拟用: <=0则为正式环境
   */
  private def choose(simulation: Int = 0): Unit = {
    log.info("共有{}只", allData.size)

    val history = if (simulation > 0) allData.map(o => TongStockHistory(o.code, o.data.dropRight(simulation))) else allData

    var list = history.map(o => IndicatorsCalculator.calc(o, option.period, option.count))

    StrategyOption.filter(option).foreach(f => list = list.filter(f))

    list.sortBy(_.avgVariance)

    log.info("满足条件的有:{}只\n{}", list.size, list.map(_.stock.code.substring(2)).mkString(";"))

    show(list, filter = true, simulation = simulation)
  }

  private def show(list: Seq[TongStockCompoundIndicators], filter: Boolean = false, detail: Boolean = false, simulation: Int = 0): Unit = {
    val currentMap = SinaQuotesApi.getData(list.map(_.stock.code)).map(o => (o.code, o)).toMap
    val historyMap = list.map(o => (o.stock.code, o)).toMap
    val map = allData.map(o => (o.code, o.data.takeRight(simulation))).toMap

    list.foreach(_show)

    def _show(indicators: TongStockCompoundIndicators): Unit = {
      val code = indicators.stock.code

      val history = historyMap(code)
      val now = currentMap(code)

      val except = history.indicators.map(_.avgPrice).take(2).min

      val simulationPrice = percentageToDecimal(indicators.stock.data.last.closingPrice)
      val currentPrice = now.currentPrice.toDouble
      val price = if (simulation > 0) simulationPrice else currentPrice

      val profit = option.minProfitRatio.forall(d => except >= (100 + d) * price / 100)
      val pass = !now.name.contains("ST") && profit

      if (!pass && filter) return

      if (detail) log.info(history.indicators.map(_.toString).mkString("\n"))

      if (simulation > 0) {
        val max = map(code).maxBy(_.closingPrice)
        val maxPrice = percentageToDecimal(max.closingPrice)
        val simulationPass = maxPrice >= except
        log.info(
          "{}\t{}\t{}\t[{}]\t买入:{} -> 预期:{}\t 验证 -> 近期高点:[{}] {}",
          if (simulationPass) "+++" else "---",
          code,
          f"${now.name}%-7s",
          s"${history.stock.data.last.date}",
          f"$price%5.2f",
          f"$except%5.2f",
          max.date,
          maxPrice
        )
      } else {
        log.info(
          "{}\t{}\t{}\t买入:{} -> 预期:{}\t盈利:{}\t{}",
          if (pass) "+++" else "---",
          code,
          f"${now.name}%-7s",
          f"$price%2.2f",
          f"$except%2.2f",
          f"${(except - price) * 100 / price}%.2f${"%"}",
          history.toString
        )
      }
    }
  }
}
