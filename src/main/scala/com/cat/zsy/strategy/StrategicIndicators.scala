package com.cat.zsy.strategy
import com.cat.zsy.strategy.StrategyFactory.intToDecimal

case class StrategicIndicators(
    /**
     * 统计周期
     */
    period: Int,

    /**
     * 统计区间是否足够
     */
    enough: Boolean,

    /**
     * 均价
     */
    avg: Int,

    /**
     * 当前(最后)价
     */
    current: Int,

    /**
     * 震荡曲线
     */
    oscillation: Seq[Int],

    /**
     * 振幅
     */
    amplitude: Seq[Int]
) {
  override def toString: String =
    s"""
      |统计区间:${period}天(区间长度:${if (enough) "足够" else "不足"})
      |当前价:${intToDecimal(current)}\t均价:${intToDecimal(avg)}\t最长低谷期:${maxDownOscillation}\t平均振幅:${intToDecimal(avgAmplitude)}
      |震荡曲线:${oscillation.mkString(",")}
      |振幅曲线:${amplitude.takeRight(10).mkString(",")}
      |""".stripMargin

  def maxDownOscillation: Int = -oscillation.min
  def avgAmplitude: Int = amplitude.map(Math.abs).sum * 100 / amplitude.size
}
