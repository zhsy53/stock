package com.cat.zsy.strategy
import com.cat.zsy.util.StockUtils._

/**
 * 指标
 */
case class TongStockIndicators(
    /**
     * 统计周期:天
     */
    period: Int,

    /**
     * 统计区间是否足够
     */
    enough: Boolean = false,

    /**
     * 均价
     */
    avgPrice: Double = 0,

    /**
     * 均值方差
     */
    avgVariance: Double = 0,

    /**
     * 收盘价
     */
    closingPrice: Double = 0,

    /**
     * 最高/低价
     */
    highestPrice: Double = 0,
    lowestPrice: Double = 0,

    /**
     * 震荡曲线:天
     */
    oscillation: Seq[Int] = Seq.empty,

    /**
     * 振幅百分比
     */
    amplitude: Seq[Double] = Seq.empty
) {
  // 最大低谷
  def maxDownOscillation: Int = -oscillation.min
  // 平均低谷
  def avgDownOscillation: Double = avg(oscillation.filter(_ < 0).map(-_))

  // 平均振幅
  def avgAmplitude: Double = avg(amplitude.map(Math.abs))
}
