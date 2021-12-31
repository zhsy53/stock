package com.cat.zsy.strategy
import com.cat.zsy.domain.StockSeq

case class StockStrategicIndicators(
    stock: StockSeq,
    indicators: Seq[StrategicIndicators]
) {
  override def toString: String =
    s"""
      |${stock.code}
      |${indicators.map(_.toString).mkString("")}
      |----------------------------------------------
      |""".stripMargin

  def maxDownOscillation: Int = indicators.map(_.maxDownOscillation).max
  def avgDownOscillation: Int = indicators.map(_.maxDownOscillation).sum / indicators.size
}
