package com.cat.zsy.strategy
import com.cat.zsy.util.StockUtils
import com.cat.zsy.util.StockUtils._

import scala.math.BigDecimal.RoundingMode.DOWN

/**
 * 指标
 * 价格均 * 100
 */
case class TongStockIndicators(
    /**
     * 统计周期:天
     */
    period: Int,

    /**
     * 统计区间是否足够
     */
    enough: Boolean,

    /**
     * 均价
     */
    avgPrice: Int,

    /**
     * 收盘价
     */
    closingPrice: Int,

    /**
     * 震荡曲线:天
     */
    oscillation: Seq[Int],

    /**
     * 振幅 * 100
     */
    amplitude: Seq[Double]
) {
  override def toString: String =
    s"""
      |统计周期(天):${period}(周期:${if (enough) "足够" else "不足"})
      |均价:${percentageToDecimal(avgPrice)}\t最长低谷(天):$maxDownOscillation\t平均振幅(%):$avgAmplitude
      |震荡曲线(天):${formatArray(oscillation)}
      |振幅曲线(%):${formatArray(amplitude.takeRight(10).map(BigDecimal.valueOf(_).setScale(2, DOWN)))}
      |""".stripMargin

  // 最大低谷
  def maxDownOscillation: Int = -oscillation.min

  // 平均振幅
  def avgAmplitude: BigDecimal = StockUtils.avg(amplitude).setScale(2, DOWN)
}
