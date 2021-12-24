package com.cat.zsy

import com.cat.zsy.domain.StockSeq
import com.cat.zsy.strategy.StrategyFactory
import org.slf4j.LoggerFactory

object Application extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  private val dir = "D:\\stock"

  val begin = System.currentTimeMillis()

  // show()
  choose()

  val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

  def choose(): Unit = {
    val list = DataFactory.listAllDataFromFilepath(dir)

    val filterList = List(
      list.filter(o => StrategyFactory.oscillation(o, 100, 12))
      // list.filter(o => StrategyFactory.oscillation(o, 200, 18))
      // list.filter(o => StrategyFactory.oscillation(o, 300, 16))
    )

    val result = filterList.reduce((a, b) => a.intersect(b))

    log.info(s"满足条件的有:${result.size}只:")

    result
      .map(o => (o.code.substring(0, 2), o.code))
      .groupBy(tuple => tuple._1)
      .map(o => o._2.map(t => t._2.substring(2)).mkString(";"))
      .foreach(log.info)
  }

  def show(): Unit = {
    val code = "sz" + "000591"
    // val code = "sh" + "600085"
    val data = DataFactory.getStockSeqByCodeFromDir(code, dir)

    val seq = StockSeq(code, data)
    log.info(seq.toString)

    log.info(StrategyFactory.oscillation(seq, 100, detail = true).toString)
  }
}
