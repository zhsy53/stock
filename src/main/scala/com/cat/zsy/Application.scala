package com.cat.zsy

import com.cat.zsy.domain.StockSeq
import com.cat.zsy.strategy.StrategyFactory
import org.slf4j.LoggerFactory

object Application extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  private val dir = "D:\\stock"

  val begin = System.currentTimeMillis()

//  show()
  choose()
//  show(codes)
//  now()

  val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

  def choose(): Unit = {
    val list = DataFactory.listAllDataFromFilepath(dir)

    val r = list
      .map(o => StrategyFactory.oscillations(o, List(100, 200, 300, 400)))
      .filter(_.indicators.nonEmpty) // 已退市的垃圾股
      .filter(o => o.indicators.count(_.enough) >= 2) // 统计区间不足的
      .filter(_.avgDownOscillation <= 35)

    r.map(_.toString)
      .foreach(log.info)

    log.info("满足条件的有:{}只", r.size)
    log.info(r.map(_.stock.code.substring(2)).sorted.mkString(";"))
  }

  def show(codes: Seq[String]): Unit = {
    val nameCodes = List(
      ("银泰黄金", "000975"), // 2
      ("通富微电", "002156"), // 1
      ("高盟新材", "300200"), // 1
      ("金太阳", "300606"), // 1
      ("王府井", "600859"), // 3
      ("中国太保", "601601"), // 1
      ("百合花", "603823"), // 1
      ("华生科技", "605180"), // 2
      ("三花智控", "002050"), // 2
      ("圣元环保", "300867")
    )
//    深圳
//    val code = "sz" + "300718" // 长盛轴承
//    val code = "sz" + "300461" // 田中精机
//    val code = "sz" + "002043" // 兔宝宝
//    val code = "sz" + "002278" // 神开股份
//    val code = "sz" + "300200" // 高盟新材
//    val code = "sz" + "300867" // 圣元环保
//    val code = "sz" + "300606" // 金太阳
//    val code = "sz" + "300634" //
    // 上海
//    val code = "sh" + "603535" // 嘉诚国际
//    val code = "sh" + "688466" // 金科环境
//    val code = "sh" + "601727" // 上海电气

    codes.foreach(showList)

    def showList(code: String): Unit = {
      val preCode = if (code.startsWith("6")) "sh" + code else "sz" + code
      val data = DataFactory.getStockSeqByCodeFromDir(preCode, dir)

      val seq = StockSeq(code, data)

      log.info(StrategyFactory.oscillation(seq, 100, detail = true).toString)
      log.info(StrategyFactory.oscillation(seq, 200, detail = true).toString)
      log.info(StrategyFactory.oscillation(seq, 300, detail = true).toString)
      log.info(StrategyFactory.oscillation(seq, 400, detail = true).toString)
      log.info(StrategyFactory.oscillation(seq, 500, detail = true).toString)
      log.info(StrategyFactory.oscillation(seq, 600, detail = true).toString)
      log.info("------------------------------------------\n")
    }

  }

  def now(): Unit = {
    val list = DataFactory.listAllDataFromFilepath(dir)

    val r = list
      .filter(o => StrategyFactory.now(o, 300, Option.empty, 3, 10))
      .map(_.code.substring(2))
      .mkString(";")

    log.info(r)
  }

  def print(): Unit = {
    val sz = "300461;002050;300718;002043;000063;000888;002851;000938;002278;300200;300867;"
      .split(";")
      .distinct
      .toList
      .sorted
      .map(c => "sz" + c)
      .map(c => StockSeq(c, DataFactory.getStockSeqByCodeFromDir(c, dir)))
      .map(d => ())
  }
}
