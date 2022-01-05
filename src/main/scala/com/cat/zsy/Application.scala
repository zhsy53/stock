package com.cat.zsy

import com.cat.zsy.api.{SinaQuotesApi, TongStockApi}
import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.strategy.OscillationCalculator
import com.cat.zsy.util.StockUtils
import com.cat.zsy.util.StockUtils.intToDecimal
import org.slf4j.LoggerFactory

object Application extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  private val dir = "D:\\stock"

  val begin = System.currentTimeMillis()

  val codes =
    "\"sh600178\";\"sh600258\";\"sh600706\";\"sh600845\";\"sh600960\";\"sh603112\";\"sh603607\";\"sh603969\";\"sh605255\";\"sh688196\";\"sz000010\";\"sz000063\";\"sz000530\";\"sz000712\";\"sz000888\";\"sz000927\";\"sz000938\";\"sz002043\";\"sz002050\";\"sz002207\";\"sz002236\";\"sz002278\";\"sz002370\";\"sz002703\";\"sz002831\";\"sz002851\";\"sz300021\";\"sz300094\";\"sz300125\";\"sz300200\";\"sz300229\";\"sz300461\";\"sz300481\";\"sz300626\";\"sz300680\";\"sz300718\";\"sz300806\";"

//  val codes = "600258;600706;600845;600960;603112;603605;603607;603969;605255;688196";

//  val codes = "000938;300200;002207;002851;300718;600845;002851;603357;002788"
  show(codes.split(";").map(_.substring(3, 9)).toList)
//  choose()

  val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

  private def choose(): Unit = {
    val list = TongStockApi.listAllDataFromFilepath(dir)

    val r = list
      .map(o => OscillationCalculator.calcList(o, List(100, 200, 300, 400)))
      .filter(_.indicators.nonEmpty) // 已退市的垃圾股
      .filter(o => o.indicators.count(_.enough) >= 3) // 统计区间不足的
      .filter(_.avgDownOscillation <= 40) // 低谷驻留时间不超过**
      .filter(_.avgAmplitude >= 80) // 平均振幅

    r.map(_.toString).foreach(log.info)

    log.info("满足条件的有:{}只", r.size)
    log.info(r.map(_.stock.code.substring(2)).sorted.mkString(";"))
  }

  def show(codes: Seq[String]): Unit = {
    val currentMap = SinaQuotesApi.getData(codes).map(o => (o.code.substring(2), o)).toMap
    codes.foreach(showList)

    def showList(code: String): Unit = {
      val preCode = StockUtils.fixCode(code)
      val data = TongStockApi.getStockSeqByCodeFromDir(preCode, dir)

      val seq = TongStockHistory(code, data)

      val r = OscillationCalculator.calcList(seq, List(100, 200, 300, 400, 500, 600, 700, 800))

      val current = currentMap(r.stock.code)
      val avg = intToDecimal(r.indicators.head.avg)

      log.info(
        "{}\t{}\t{}\t现价:{}\t均值:{}\t需要{}天\t均值曲线:{}",
        if (avg > current.currentPrice) "+" else "",
        code,
        current.name,
        current.currentPrice,
        avg,
        r.indicators.head.maxDownOscillation,
        r.indicators.map(_.avg).map(intToDecimal).mkString("[", ", ", "]")
      )
    }
  }
}
