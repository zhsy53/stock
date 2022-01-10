package com.cat.zsy.strategy
import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.util.StockUtils._

import scala.math.BigDecimal.RoundingMode.{DOWN, UP}

case class TongStockCompoundIndicators(stock: TongStockHistory, indicators: Seq[TongStockIndicators]) {
  override def toString: String =
    s"""
      |${stock.code}
      |${indicators.map(_.toString).mkString("")}
      |收盘价:${percentageToDecimal(indicators.head.closingPrice)} \t 均价:${percentageToDecimal(indicators.head.avgPrice)} \t 均价方差:${(avgVariance / 100).setScale(2, UP)}
      |最长低谷:$maxDownOscillation \t 平均低谷:$avgDownOscillation
      |均价曲线:${formatArray(indicators.map(_.avgPrice).map(percentageToDecimal))}
      |低谷曲线:${formatArray(indicators.map(_.maxDownOscillation))}
      |均幅曲线:${formatArray(indicators.map(_.avgAmplitude))}
      |----------------------------------------------
      |""".stripMargin

  // 最长低谷
  def maxDownOscillation: Int = indicators.map(_.maxDownOscillation).max

  // 平均低谷
  def avgDownOscillation: BigDecimal = avg(indicators.map(_.maxDownOscillation)).setScale(0, UP)

  // 平均振幅
  def avgAmplitude: BigDecimal = avg(indicators.flatMap(_.amplitude).map(Math.abs)).setScale(2, DOWN)

  // 均值方差
  def avgVariance: BigDecimal = variance(indicators.map(_.avgPrice)).setScale(2, UP)

  def log: String = s"均价:${percentageToDecimal(indicators.map(_.avgPrice).min)}\t" +
//    f"均价方差:${(avgVariance / 100)}%.2f\t" +
    s"均价曲线:${formatArray(indicators.map(_.avgPrice).map(percentageToDecimal))}\t" +
    f"平均低谷:$avgDownOscillation%3.0f\t最长低谷:$maxDownOscillation%3d\t" +
//    f"平均振幅:${avgDownOscillation / 100}%.2f\t" +
    s"低谷曲线:${formatArray(indicators.map(_.maxDownOscillation))}\t" +
    s"均幅曲线:${formatArray(indicators.map(_.avgAmplitude))}"
}
