package com.cat.zsy

import com.cat.zsy.api.{SinaQuotesApi, TongStockApi}
import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.strategy.OscillationCalculator
import com.cat.zsy.util.StockUtils
import com.cat.zsy.util.StockUtils.intToDecimal
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
  private val log = LoggerFactory.getLogger(this.getClass)

  private val dir = "D:\\stock"

  val begin = System.currentTimeMillis()

  val codes = choose()

//  val codes = "300369;300117;002214;000878;300719;600739;603186;600988;"
//    .split(";")
//    .distinct
//    .sorted
//    .toList

  log.info(codes.mkString(";"))

  show(codes, detail = false)

  val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

  private def choose(): Seq[String] = {
    val list = TongStockApi.listAllDataFromFilepath(dir)

    val r = list
      .map(o => OscillationCalculator.calcList(o, List(100, 200, 300, 400)))
      .filter(_.indicators.nonEmpty) // 已退市的垃圾股
      .filter(o => o.indicators.count(_.enough) >= 3) // 统计区间不足的
      .filter(_.avgDownOscillation <= 42) // 低谷驻留时间不超过**
      .filter(_.avgAmplitude >= 100) // 平均振幅
//      .filter(o => o.indicators.head.avg >= o.indicators.head.last * 1.08) // 盈利空间

//    r.map(_.toString).foreach(log.info)

    log.info("满足条件的有:{}只\n{}", r.size, r.map(_.stock.code.substring(2)).sorted.mkString(";"))

    r.map(_.stock.code.substring(2)).sorted
  }

  def show(codes: Seq[String], detail: Boolean = false): Unit = {
    val currentMap = SinaQuotesApi.getData(codes).map(o => (o.code.substring(2), o)).toMap
    codes.foreach(showList)

    def showList(code: String): Unit = {
      val preCode = StockUtils.fixCode(code)
      val data = TongStockApi.getStockSeqByCodeFromDir(preCode, dir)

      val seq = TongStockHistory(code, data)

      val r = OscillationCalculator.calcList(seq, List(100, 200, 300, 400, 500, 600, 700, 800))

      val current = currentMap(r.stock.code)
      val avg = intToDecimal(r.indicators.head.avg)

      if (detail) {
        log.info(r.toString)
      }

      if (avg >= current.currentPrice * 1.05 && current.currentPrice <= 60) {
        log.info(
          "{}\t{}\t{}\t现价:{}\t均值:{}\t需要{}天\t均值曲线:{}\t震荡曲线:{}",
          if (avg > current.currentPrice) "+" else "",
          code,
          current.name,
          current.currentPrice,
          avg,
          r.indicators.head.maxDownOscillation,
          r.indicators.map(_.avg).map(intToDecimal).mkString("[", ", ", "]"),
          r.indicators.map(_.maxDownOscillation).mkString("[", ", ", "]")
        )
      }
    }
  }
}
