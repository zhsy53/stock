package com.cat.zsy

import com.cat.zsy.api.{SinaQuotesApi, TongStockApi}
import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.strategy._
import com.cat.zsy.util.StockUtils.{divPercentage, percentageToDouble}
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

  val begin = System.currentTimeMillis()

  val allData = TongStockApi.listAllDataFromFilepath(dir)

//  Range(30 + 60, 30, -1).foreach(t => executeForSimulation(backtracking = t, observation = 33, detail = true))

  executeForCurrent(always = false)

  val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

  /**
   * @param backtracking 回溯天数
   * @param observation 观察天数 >= backtracking
   */
  def executeForSimulation(
      duration: StrategyDuration = StrategyDuration.default,
      filter: StrategyFilter = StrategyFilter.default,
      backtracking: Int = 23,
      observation: Int = 22,
      detail: Boolean = false
  ): Unit = {
    val list = executeStrategy(duration, filter, backtracking)

    val backtrackingMap = list.map(o => (o.stock.code, o)).toMap
    val observationMap = allData.map(o => (o.code, o.data.takeRight(backtracking).take(observation))).toMap

    var passCount = 0
    var simulationPassCount = 0
    var guaranteedPassCount = 0

    list.foreach(_show)

    log.info(
      "测试[{}天前]\t盈利:{}/{} -> {}%\t保本:{}/{} -> {}%\n",
      backtracking,
      simulationPassCount,
      passCount,
      f"${divPercentage(simulationPassCount, passCount)}%5.2f",
      guaranteedPassCount,
      passCount,
      f"${divPercentage(guaranteedPassCount, passCount)}%5.2f"
    )

    def _show(indicators: TongStockCompoundIndicators): Unit = {
      val code = indicators.stock.code

      val history = backtrackingMap(code)

      val except = history.indicators.map(_.avgPrice).take(2).min

      val price = percentageToDouble(indicators.stock.data.last.closingPrice)

      val profit = filter.minProfitRatio.forall(d => except >= (100 + d) * price / 100)

      // 根据策略决定是否进场
      val pass = profit

      if (!pass) return

      passCount += 1

      val observation = observationMap(code)

      // 观察期内最高收盘价
      val allMax = observation.maxBy(_.highestPrice)
      val allMaxPrice = percentageToDouble(allMax.closingPrice)
      val simulationPass = allMaxPrice >= except

      // 观察期倒数4-1的3天内的最高收盘价
      val lastMax = observation.takeRight(4).take(4).maxBy(_.closingPrice)
      val lastMaxPrice = percentageToDouble(lastMax.closingPrice)
      val guaranteedPass = lastMaxPrice >= price

      if (simulationPass) simulationPassCount += 1
      if (simulationPass || guaranteedPass) guaranteedPassCount += 1

      if (detail) {
        log.info(
          "{}{}\t{}\t{}\t买入:[{}] {} -> {}\t 验证 -> 高点:[{}] {} 抛售点:[{}] {}",
          if (simulationPass) "++" else "--",
          if (guaranteedPass) "++" else "--",
          code,
          f"${"todo"}%-7s",
          s"${history.stock.data.last.date}",
          f"$price%5.2f",
          f"$except%5.2f",
          allMax.date,
          f"$allMaxPrice%5.2f",
          lastMax.date,
          f"$lastMaxPrice%5.2f"
        )
      }
    }
  }

  /**
   * @param truncate 截断最新数据数 -> 模拟用: <=0则为正式环境
   */
  private def executeStrategy(duration: StrategyDuration = StrategyDuration.default, filter: StrategyFilter = StrategyFilter.default, truncate: Int = 0): Seq[TongStockCompoundIndicators] = {
    var list = loadData(duration, truncate)

    log.debug("共有待选[{}]只", list.size)

    StrategyFilter.doFilter(filter).foreach(f => list = list.filter(f))

    list.sortBy(_.avgVariance)

    log.debug("满足当前策略(不考虑入场时机)的有[{}]只:\n{}", list.size, list.map(_.stock.code.substring(2)).mkString(";"))

    list
  }

  private def loadData(duration: StrategyDuration = StrategyDuration.default, truncate: Int = 0): Seq[TongStockCompoundIndicators] = {
    log.debug("共有{}只", allData.size)

    val history = if (truncate > 0) allData.map(o => TongStockHistory(o.code, o.data.dropRight(truncate))) else allData

    history.map(o => IndicatorsCalculator.calc(o, duration.period, duration.count))
  }

  /**
   * @param always 无论如何总显示
   * @param detail 显示明细
   */
  def executeForCurrent(duration: StrategyDuration = StrategyDuration.default, filter: StrategyFilter = StrategyFilter.default, always: Boolean = false, detail: Boolean = false): Unit = {
    val list = executeStrategy(duration, filter)

    val currentMap = SinaQuotesApi.getData(list.map(_.stock.code)).map(o => (o.code, o)).toMap
    val historyMap = list.map(o => (o.stock.code, o)).toMap

    list.foreach(_show)

    def _show(indicators: TongStockCompoundIndicators): Unit = {
      val code = indicators.stock.code

      val history = historyMap(code)
      val current = currentMap(code)

      val except = history.indicators.map(_.avgPrice).take(2).min
      val price = current.currentPrice

      // 盈利
      val profit = filter.minProfitRatio.forall(d => except >= (100 + d) * price / 100)

      val pass = !current.name.contains("ST") && profit

      if (!pass && !always) return

      if (detail) log.info(history.indicators.map(_.toString).mkString("\n"))

      log.info(
        "{}\t{}\t{}\t买入:{} -> {} [{}%]\t{}",
        if (pass) "+++" else "---",
        code,
        f"${current.name}%-7s",
        f"$price%5.2f",
        f"$except%5.2f",
        f"${(except - price) * 100 / price}%6.2f",
        history.toString
      )
    }
  }
}
