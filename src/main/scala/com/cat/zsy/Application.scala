package com.cat.zsy

import com.cat.zsy.api.{SinaQuotesApi, TongStockApi}
import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.strategy.OscillationCalculator
import com.cat.zsy.util.StockUtils
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
  val maxDownOscillation = 44
  val minAmplitude = 0.01
  val profitRatio = 0.04

  val begin = System.currentTimeMillis()

  val codes = list()

  val codes1 =
    "002043;002050;000888;002207;300718;300461;300200;000063;300094;002851;000938;000712;000927;300719;300369;601919;600988;688981;600739;"
      .split(";")
      .distinct
      .sorted
      .toList

  log.info("满足条件的有:{}只\n{}", codes.size, codes.mkString(";"))

  choose(codes, table = false, detail = false)
  val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

  /**
   * @param table 满足条件时以表格展示
   * @param detail 是否显示详情
   */
  def choose(codes: Seq[String], table: Boolean = false, detail: Boolean = true): Unit = {
    val currentMap = SinaQuotesApi.getData(codes).map(o => (o.code.substring(2), o)).toMap
    codes.foreach(showList)

    def showList(code: String): Unit = {
      val preCode = StockUtils.fixCode(code)
      val data = TongStockApi.getStockSeqByCodeFromDir(preCode, dir)

      val seq = TongStockHistory(code, data)

      val r = OscillationCalculator.calcList(seq, chooseDuration)

      val current = currentMap(r.stock.code)
      val avg = StockUtils.percentageToDecimal(r.indicators.map(_.avgPrice).min)

      val currentPrice = current.currentPrice
      if (detail) {
        log.info(
          "{}\t现价:{}\n{}",
          f"${current.name}%-6s",
          f"$currentPrice%2.2f",
          r.toString
        )
      }

      val pass = avg >= currentPrice * (1 + profitRatio) && currentPrice <= 40
      if (table || pass) {
        log.info(
          "{}\t{}\t{}\t现价:{}\t预期盈利:{}\t{}",
          if (pass) "+++" else "---",
          code,
          f"${current.name}%-7s",
          f"$currentPrice%2.2f",
          f"${(avg - currentPrice) / currentPrice}%.2f",
          r.log
        )
      }
    }
  }

  private def list(): Seq[String] = {
    TongStockApi
      .listAllDataFromFilepath(dir)
      .map(o => OscillationCalculator.calcList(o, listDuration))
      .filter(_.indicators.nonEmpty) // 已退市的
      .filter(o => o.indicators.count(_.enough) >= 3) // 统计区间不足的
      .filter(o => o.avgDownOscillation <= maxDownOscillation || o.maxDownOscillation <= maxDownOscillation * 2) // 低谷驻留时间不超过**
      .filter(_.avgAmplitude >= minAmplitude * 100) // 平均振幅
      .filter(o => o.indicators.map(_.avgPrice).min >= o.indicators.head.closingPrice * 1.04) // 盈利空间
//      .filter(_.avgVariance <= 0.25 * 100)
//      .sortBy(_.avgVariance)
      .map(_.stock.code.substring(2))
  }
}
