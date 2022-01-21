package com.cat.zsy.strategy.executor
import com.cat.zsy.api.SinaQuotesApi
import com.cat.zsy.cache.DataFactory
import com.cat.zsy.domain.{TongStockElement, TongStockHistory}
import com.cat.zsy.strategy._
import org.slf4j.LoggerFactory

import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable

object PriceExecutor {
  private val log = LoggerFactory.getLogger(this.getClass)

//  /**
//   * @param backtracking 回溯天数
//   * @param observation 观察天数 >= backtracking
//   */
//  def executeForSimulation(
//      duration: StatisticsDuration = StatisticsDuration(),
//      filter: StrategyFilter = StrategyFilter.default,
//      backtracking: Int = 23,
//      observation: Int = 22,
//      minProfitRatio: Option[Double] = Option(4.5),
//      detail: Boolean = false
//  ): Unit = {
//    val list = executeStrategy(duration, filter, backtracking)
//
//    val backtrackingMap = list.map(o => (o.stock.code, o)).toMap
//    val observationMap = allData.map(o => (o.code, o.data.takeRight(backtracking).take(observation))).toMap
//
//    var passCount = 0
//    var simulationPassCount = 0
//    var guaranteedPassCount = 0
//
//    list.foreach(_show)
//
//    log.info(
//      "测试[{}天前]\t盈利:{}/{} -> {}%\t保本:{}/{} -> {}%",
//      backtracking,
//      simulationPassCount,
//      passCount,
//      f"${divPercentage(simulationPassCount, passCount)}%5.2f",
//      guaranteedPassCount,
//      passCount,
//      f"${divPercentage(guaranteedPassCount, passCount)}%5.2f"
//    )
//
//    def _show(indicators: TongStockCompoundIndicators): Unit = {
//      val code = indicators.stock.code
//
//      val history = backtrackingMap(code)
//
//      val except = history.indicators.map(_.avgPrice).take(2).min
//
//      val price = percentageToDouble(indicators.stock.data.last.closingPrice)
//
//      val profit = minProfitRatio.forall(d => except >= (100 + d) * price / 100)
//
//      // 根据策略决定是否进场
//      val pass = profit
//
//      if (!pass) return
//
//      passCount += 1
//
//      val observation = observationMap(code)
//
//      // 观察期内最高收盘价
//      val allMax = observation.maxBy(_.highestPrice)
//      val allMaxPrice = percentageToDouble(allMax.closingPrice)
//      val simulationPass = allMaxPrice >= except
//
//      // 观察期倒数4-1的3天内的最高收盘价
//      val lastMax = observation.takeRight(4).take(4).maxBy(_.closingPrice)
//      val lastMaxPrice = percentageToDouble(lastMax.closingPrice)
//      val guaranteedPass = lastMaxPrice >= price
//
//      if (simulationPass) simulationPassCount += 1
//      if (simulationPass || guaranteedPass) guaranteedPassCount += 1
//
//      if (detail) {
//        log.info(
//          "{}{}\t{}\t{}\t买入:[{}] {} -> {}\t 验证 -> 高点:[{}] {} 抛售点:[{}] {}",
//          if (simulationPass) "++" else "--",
//          if (guaranteedPass) "++" else "--",
//          code,
//          f"${"todo"}%-7s",
//          s"${history.stock.data.last.date}",
//          f"$price%5.2f",
//          f"$except%5.2f",
//          allMax.date,
//          f"$allMaxPrice%5.2f",
//          lastMax.date,
//          f"$lastMaxPrice%5.2f"
//        )
//      }
//    }
//  }

  /**
   * @param always 无论如何总显示
   * @param detail 显示明细
   */
  def executeForCurrent(
      h: Option[DataHandler[Seq[TongStockElement]]] = Option.empty,
      f: Option[DataFilter[TongStockHistory]] = Option.empty,
      profit: Option[Double] = Option(4.5),
      always: Boolean = false,
      detail: Boolean = false
  ): Unit = {
    val list = handleByElementsAndFilter(h, f)

    val currentMap = SinaQuotesApi.getData(list.map(_.code)).map(o => (o.code, o)).toMap

    list.foreach(_show)

    def _show(t: TongStockHistory): Unit = {
      val code = t.code

      val history = TongStockPriceIndicator(t.data)

      val current = currentMap(code)

      val except = history.avgPrice
      val price = current.currentPrice
      if (price == 0) {
        log.error("---\t{} error", code)
        return
      }

      // 盈利
      val pass = !current.name.contains("ST") && profit.forall(d => except >= (100 + d) * price / 100) && !current.name.contains("ST")

      if (!pass && !always) return

      if (detail) log.info(history.toString)

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

  def handleByElementsAndFilter(h: Option[DataHandler[Seq[TongStockElement]]] = Option.empty, f: Option[DataFilter[TongStockHistory]] = Option.empty): Seq[TongStockHistory] = {
    val data = handleByElements(h)

    log.info("共有待选[{}]只", data.size)

    val list = f.map(o => data.par.filter(o.filter).toList).getOrElse(data)

    log.info("满足当前策略(不考虑入场时机)的有 [{}] 只:\n{}", list.size, list.map(_.code.substring(2)).mkString(";"))

    list
  }

  def handleByElements(h: Option[DataHandler[Seq[TongStockElement]]] = Option.empty): Seq[TongStockHistory] = handByHistory(h.map(TruncateDataHandler.mapper))

  def handByHistory(h: Option[DataHandler[TongStockHistory]] = Option.empty): Seq[TongStockHistory] = h.map(h => DataFactory.data.map(h.handle)).getOrElse(DataFactory.data)
}
