//package com.cat.zsy.strategy
//import com.cat.zsy.domain.TongStockHistory
//import com.cat.zsy.strategy.config.StatisticsDuration
//import com.cat.zsy.util.StockUtils.{avg, formatArray, increase, variance}
//
//case class TongStockCompoundIndicators(stock: TongStockHistory, duration: StatisticsDuration, indicators: Seq[TongStockIndicators]) {
//  override def toString: String = f"均价:$avgPrice%.2f\t" +
//    f"方差:${avgVariance * 100}%4.2f\t" +
////    f"日涨:${avgIncrease * 1000}%4.2f\t" +
//    f"振幅:$avgAmplitude%9.6f\t" +
//    f"低谷: ${Math.max(-1, -indicators.head.oscillation.last)} -> $avgTrough%2.0f/$maxTrough%2d\t" +
//    s"均价曲线:${formatArray(indicators.map(_.avgPrice).map(String.format("%.2f", _)))}\t" +
//    s"低谷曲线:${formatArray(indicators.map(_.maxTrough))}"
//
//  // 最长低谷
//  def maxTrough: Int = indicators.map(_.maxTrough).max
//
//  // 平均低谷
//  def avgTrough: Double = avg(indicators.map(_.avgTrough))
//
//  // 均价
//  def avgPrice: Double = (indicatorsData.map(_.closingPrice).sum / 100.0) / indicatorsData.size
//
//  // 均价方差
//  def avgVariance: Double = variance(indicatorsData.map(_.closingPrice))
//
//  // 平均增幅
//  def avgIncrease: Double = avg(increase(indicatorsData.map(_.closingPrice)))
//
//  // 平均振幅(%)
//  def avgAmplitude: Double = avg(indicatorsData.map(o => (o.highestPrice - o.lowestPrice) * 100 / o.openingPrice))
//
//  def enough: Boolean = stock.data.size >= duration.period * duration.count
//
//  private def indicatorsData = stock.data.takeRight(duration.period * duration.count)
//}
