package com.cat.zsy.strategy
import com.cat.zsy.util.StockUtils._

/**
 * 指标
 * @param period 统计周期:天
 * @param enough 统计区间是否足够
 * @param oscillation 震荡曲线:天
 * @param avgAmplitude 平均振幅
 */
case class TongStockIndicators(
    period: Int,
    enough: Boolean = false,
    avgPrice: Double = 0,
    highestPrice: Double = 0,
    lowestPrice: Double = 0,
    oscillation: Seq[Int] = Seq.empty,
    avgAmplitude: Double = 0
) {
  // 最大低谷
  def maxTrough: Int = -oscillation.min
  // 平均低谷
  def avgTrough: Double = avg(oscillation.filter(_ < 0).map(-_))
}
