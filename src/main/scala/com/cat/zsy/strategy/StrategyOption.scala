package com.cat.zsy.strategy

import com.cat.zsy.util.StockUtils.month

import scala.collection.mutable.ListBuffer

case class StrategyDuration(
    period: Int, // 统计频率:天
    count: Int // 统计周期:统计频率的整数倍
)

case class StrategyFilter(
    minCount: Option[Int] = Option.empty, // 最少周期
    maxPrice: Option[Double] = Option.empty, // 最高价
    maxAvgDwnOscillation: Option[Int] = Option.empty, // 最长均值低谷期
    maxDwnOscillation: Option[Int] = Option.empty, // 最长低谷期
    minAmplitude: Option[Double] = Option.empty, // 最小平均振幅
    maxAvgVariance: Option[Double] = Option.empty, // 最大均值方差
    maxAvgDownRatio: Option[Double] = Option.empty, // 最大均值增/减幅
    minProfitRatio: Option[Double] = Option.empty // 最小盈利比(%)
)

object StrategyDuration {
  private val year = 2
  private val monthCount = 3

  def default: StrategyDuration = StrategyDuration(month * monthCount, year * 12 / monthCount)
}

object StrategyFilter {
  def default: StrategyFilter = StrategyFilter(
    Option(6),
    Option(40.0),
    Option(month * 1.5.toInt),
    Option(month * 1.5.toInt),
    Option(3.0),
    Option(0.01),
    Option(0.12),
    Option(3.0)
  )

  def doFilter(filter: StrategyFilter): Seq[TongStockCompoundIndicators => Boolean] = {
    val buffer = ListBuffer[TongStockCompoundIndicators => Boolean]()

    filter.minCount.map(o => { t: TongStockCompoundIndicators => t.indicators.count(_.enough) >= o }).foreach(buffer += _)

    filter.maxPrice.map(o => { t: TongStockCompoundIndicators => t.indicators.head.avgPrice <= o }).foreach(buffer += _)

    // or
    if (filter.maxAvgDwnOscillation.isDefined && filter.maxAvgDwnOscillation.isDefined) {
      buffer += { t: TongStockCompoundIndicators => t.avgDownOscillation <= filter.maxAvgDwnOscillation.get || t.maxDownOscillation <= filter.maxAvgDwnOscillation.get }
    } else {
      filter.maxAvgDwnOscillation.map(o => { t: TongStockCompoundIndicators => t.avgDownOscillation <= o })
      filter.maxDwnOscillation.map(o => { t: TongStockCompoundIndicators => t.maxDownOscillation <= o })
    }

    filter.minAmplitude.map(o => { t: TongStockCompoundIndicators => t.avgAmplitude >= o }).foreach(buffer += _)

    // and
    filter.maxAvgVariance.map(o => { t: TongStockCompoundIndicators => t.avgVariance <= o }).foreach(buffer += _)
    filter.maxAvgDownRatio.map(o => { t: TongStockCompoundIndicators => increase(t.indicators.map(_.avgPrice)).map(Math.abs).forall(_ <= o) }).foreach(buffer += _)

    buffer.toSeq
  }

  private def increase(seq: Seq[Double]): Seq[Double] = {
    Range(1, seq.size).map(i => seq(i) / seq(i - 1)).map(_ - 1)
  }
}
