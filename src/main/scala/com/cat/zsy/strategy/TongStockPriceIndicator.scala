package com.cat.zsy.strategy
import com.cat.zsy.domain._
import com.cat.zsy.util.StockUtils._
import com.cat.zsy.util._

case class TongStockPriceIndicator(data: Seq[TongStockElement]) {
  def avgPrice: Double = (data.map(_.closingPrice).sum / 100.0) / data.size

  // 均价方差
  def avgVariance: Double = variance(data.map(_.closingPrice))

  // 平均增幅
  def avgIncrease: Double = avg(increase(data.map(_.closingPrice)))

  // 平均振幅(%)
  def avgAmplitude: Double = avg(data.map(o => (o.highestPrice - o.lowestPrice) * 100.toDouble / o.openingPrice))

  // 均值震荡曲线
  def oscillation: Seq[Int] = OscillationCalculator.oscillation(data.map(_.closingPrice), data.map(_.closingPrice).sum / data.size)

  override def toString: String = f"均价:$avgPrice%.2f\t方差:${avgVariance * 100}%.2f\t振幅:$avgAmplitude%.2f" +
    f"\t震荡 => 上下比:[${oscillation.filter(_ > 0).sum.toDouble / -oscillation.filter(_ < 0).sum}%.2f]\t最长:[${-oscillation.min}]\t${oscillation.mkString(" ")}"
}
