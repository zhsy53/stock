package com.cat.zsy.strategy.config

/**
 * @param codes 编码:不带前缀
 * @param count 统计数量
 * @param maxPrice 最高价
 * @param minAmplitude 最小平均振幅
 * @param maxAvgVariance 均值方差上限
 * @param minAvgIncrease 均值增幅下限
 * @param maxAvgIncrease 均值增幅上限
 */
case class PriceStrategyOption(
    codes: Option[Seq[String]] = Option.empty,
    count: Option[Int] = Option.empty,
    maxPrice: Option[Double] = Option.empty,
    minAmplitude: Option[Double] = Option.empty,
    maxAvgVariance: Option[Double] = Option.empty,
    minAvgIncrease: Option[Double] = Option.empty,
    maxAvgIncrease: Option[Double] = Option.empty
)
