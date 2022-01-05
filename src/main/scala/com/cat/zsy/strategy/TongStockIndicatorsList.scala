package com.cat.zsy.strategy
import com.cat.zsy.domain.TongStockHistory

case class TongStockIndicatorsList(stock: TongStockHistory, indicators: Seq[TongStockIndicators]) {
  override def toString: String =
    s"""
      |${stock.code}
      |${indicators.map(_.toString).mkString("")}
      |----------------------------------------------
      |""".stripMargin

  def maxDownOscillation: Int = indicators.map(_.maxDownOscillation).max
  def avgDownOscillation: Int = indicators.map(_.maxDownOscillation).sum / indicators.size
  def avgAmplitude: Int = {
    val list = indicators.flatMap(_.amplitude).map(Math.abs)
    list.sum * 100 / list.size
  }
}
