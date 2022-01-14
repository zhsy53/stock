package com.cat.zsy.strategy

import scala.collection.mutable.ListBuffer

case class StrategyOption(
    period: Int = 22 * 3, // 统计频率:天
    count: Int = 4 * 2, // 统计周期:统计频率的整数倍
    minCount: Option[Int] = Option.empty, // 最少周期
    maxPrice: Option[Double] = Option.empty, // 最高价
    maxAvgDwnOscillation: Option[Int] = Option.empty, // 最长均值低谷期
    maxDwnOscillation: Option[Int] = Option.empty, // 最长低谷期
    minAmplitude: Option[Double] = Option.empty, // 最小平均振幅
    maxAvgVariance: Option[Double] = Option.empty, // 最大均值方差
    maxAvgDownRatio: Option[Double] = Option.empty, // 最大均值跌幅
    minProfitRatio: Option[Double] = Option.empty // 最小盈利比(%)
)

object StrategyOption {
  private val month = 22
  private val year = 2
  def default: StrategyOption = StrategyOption(
    month * 3,
    year * 12 / 3,
    Option(year * 12 / 3),
    Option(35.0),
    Option(month * 1.1.toInt),
    Option(month * 1.5.toInt),
    Option(3),
    Option(0.014),
    Option(0.11),
    Option(4)
  )

  def filter(option: StrategyOption): Seq[TongStockCompoundIndicators => Boolean] = {
    val filters = ListBuffer[TongStockCompoundIndicators => Boolean]()

    option.minCount.map(o => { t: TongStockCompoundIndicators => t.indicators.count(_.enough) >= o }).foreach(filters += _)

    option.maxPrice.map(o => { t: TongStockCompoundIndicators => t.indicators.head.avgPrice <= o }).foreach(filters += _)

    // or
    if (option.maxAvgDwnOscillation.isDefined && option.maxAvgDwnOscillation.isDefined) {
      filters += { t: TongStockCompoundIndicators => t.avgDownOscillation <= option.maxAvgDwnOscillation.get || t.maxDownOscillation <= option.maxAvgDwnOscillation.get }
    } else {
      option.maxAvgDwnOscillation.map(o => { t: TongStockCompoundIndicators => t.avgDownOscillation <= o })
      option.maxDwnOscillation.map(o => { t: TongStockCompoundIndicators => t.maxDownOscillation <= o })
    }

    option.minAmplitude.map(o => { t: TongStockCompoundIndicators => t.avgAmplitude >= o }).foreach(filters += _)

    // and
    option.maxAvgVariance.map(o => { t: TongStockCompoundIndicators => t.avgVariance <= o }).foreach(filters += _)
    option.maxAvgDownRatio
      .map(o => { t: TongStockCompoundIndicators =>
        {
          val prices = t.indicators.map(_.avgPrice)
          Range(1, prices.size).map(i => prices(i) / prices(i - 1)).filter(_ < 1).map(1 - _).forall(_ <= o)
        }
      })
      .foreach(filters += _)

    filters.toSeq
  }
}
