package com.cat.zsy.strategy
import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.util.StockUtils.{avg, formatArray}

case class TongStockCompoundIndicators(stock: TongStockHistory, indicators: Seq[TongStockIndicators]) {
  override def toString: String = f"均价:$avgPrice%.2f\t" +
    f"方差:$avgVariance%.4f\t" +
    f"振幅:$avgAmplitude%.2f\t" +
    f"低谷: ${Math.max(-1, -indicators.last.oscillation.last)} -> $avgDownOscillation%2.0f/$maxDownOscillation%2d\t" +
    s"均价曲线:${formatArray(indicators.map(_.avgPrice).map(String.format("%.2f", _)))}\t" +
    s"低谷曲线:${formatArray(indicators.map(_.maxDownOscillation))}"

  // 最长低谷
  def maxDownOscillation: Int = indicators.map(_.maxDownOscillation).max

  // 平均低谷
  def avgDownOscillation: Double = avg(indicators.map(_.avgDownOscillation))

  // 平均振幅
  def avgAmplitude: Double = avg(indicators.map(_.avgAmplitude))

  // 均值方差均值
  def avgVariance: Double = avg(indicators.map(_.avgVariance))

  def avgPrice: Double = avg(indicators.map(_.avgPrice))

  //    f"均价方差曲线:${formatArray(indicators.map(_.avgVariance).map(String.format("%.4f", _)))}" +
//    formatArray(indicators.map(_.avgPrice).zip(indicators.map(_.avgVariance)).map(t => f"${t._1}%.2f -> ${t._2}%.4f"))

//    f"平均低谷:$avgDownOscillation%3.0f\t最长低谷:$maxDownOscillation%3d\t" +
//    f"平均振幅:$avgAmplitude%.2f\t" +
//    s"振幅曲线:${formatArray(indicators.map(_.avgAmplitude).map(String.format("%.1f", _)))}"
}
