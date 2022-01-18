package com.cat.zsy.strategy
import com.cat.zsy.util.StockUtils._

import scala.collection.mutable.ListBuffer

/**
 * @param minCount 最少窗口
 * @param maxPrice 最高价
 * @param maxAvgTrough 最长均值低谷期
 * @param maxTrough 最长低谷期
 * @param minAmplitude 最小平均振幅
 * @param maxAvgVariance 均值方差上限
 * @param minAvgIncrease 均值增幅下限
 * @param maxAvgIncrease 均值增幅上限
 */
case class StrategyFilter(
    minCount: Option[Int] = Option.empty,
    maxPrice: Option[Double] = Option.empty,
    maxAvgTrough: Option[Int] = Option.empty,
    maxTrough: Option[Int] = Option.empty,
    minAmplitude: Option[Double] = Option.empty,
    maxAvgVariance: Option[Double] = Option.empty,
    minAvgIncrease: Option[Double] = Option.empty,
    maxAvgIncrease: Option[Double] = Option.empty
)

object StrategyFilter {
  def default: StrategyFilter = StrategyFilter(
    // 周期
    Option(8),
    // 单价
    Option(30.0),

    // 低谷
    Option(month * 1.1.toInt),
    Option(month * 1.7.toInt),

    // 振幅
    Option(2.5),
    // 均值方差
    Option(0.6 / 100),
    Option.empty,
    Option.empty
  )

  def builder(filter: StrategyFilter): TongStockCompoundIndicators => Boolean = {
    val buffer = ListBuffer[TongStockCompoundIndicators => Boolean]()

    filter.minCount.map(o => { t: TongStockCompoundIndicators => t.indicators.count(_.enough) >= o }).foreach(buffer += _)

    filter.maxPrice.map(o => { t: TongStockCompoundIndicators => t.indicators.head.avgPrice <= o }).foreach(buffer += _)

    filter.maxAvgTrough.map(o => { t: TongStockCompoundIndicators => t.avgTrough <= o })
    filter.maxTrough.map(o => { t: TongStockCompoundIndicators => t.maxTrough <= o })

    filter.minAmplitude.map(o => { t: TongStockCompoundIndicators => t.avgAmplitude >= o }).foreach(buffer += _)

    filter.maxAvgVariance.map(o => { t: TongStockCompoundIndicators => t.avgVariance <= o }).foreach(buffer += _)
    filter.minAvgIncrease.map(o => { t: TongStockCompoundIndicators => t.avgIncrease >= o }).foreach(buffer += _)
    filter.maxAvgIncrease.map(o => { t: TongStockCompoundIndicators => t.avgIncrease <= o }).foreach(buffer += _)

    o => buffer.toSeq.forall(_(o))
  }
}
