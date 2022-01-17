package com.cat.zsy.strategy
import com.cat.zsy.util.StockUtils._

import scala.collection.mutable.ListBuffer

/**
 * @param minCount 最少窗口
 * @param maxPrice 最高价
 * @param maxAvgTrough 最长均值低谷期
 * @param maxTrough 最长低谷期
 * @param minAmplitude 最小平均振幅
 * @param maxAvgIncrease 均值增幅上限
 */
case class StrategyFilter(
    minCount: Option[Int] = Option.empty,
    maxPrice: Option[Double] = Option.empty,
    maxAvgTrough: Option[Int] = Option.empty,
    maxTrough: Option[Int] = Option.empty,
    minAmplitude: Option[Double] = Option.empty,
    minAvgIncrease: Option[Double] = Option.empty,
    maxAvgIncrease: Option[Double] = Option.empty
)

object StrategyFilter {
  def default: StrategyFilter = StrategyFilter(
    // 周期
    Option(6),
    // 单价
    Option(30.0),

    // 低谷
    Option(month * 1.1.toInt),
    Option(month * 1.6.toInt),

    // 振幅
    Option(3.5),
    // 均幅
    Option(0),
    Option(0.0065)
  )

  def builder(filter: StrategyFilter): TongStockCompoundIndicators => Boolean = {
    val buffer = ListBuffer[TongStockCompoundIndicators => Boolean]()

    filter.minCount.map(o => { t: TongStockCompoundIndicators => t.indicators.count(_.enough) >= o }).foreach(buffer += _)

    filter.maxPrice.map(o => { t: TongStockCompoundIndicators => t.indicators.head.avgPrice <= o }).foreach(buffer += _)

    filter.maxAvgTrough.map(o => { t: TongStockCompoundIndicators => t.avgTrough <= o })
    filter.maxTrough.map(o => { t: TongStockCompoundIndicators => t.maxTrough <= o })

    filter.minAmplitude.map(o => { t: TongStockCompoundIndicators => t.avgAmplitude >= o }).foreach(buffer += _)

    filter.maxAvgIncrease.map(o => { t: TongStockCompoundIndicators => between(t.avgIncrease, 0, o) }).foreach(buffer += _)

    o => buffer.toSeq.forall(_(o))
  }
}
