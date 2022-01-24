package com.cat.zsy.strategy.config
import com.cat.zsy.util.MathUtils.month

/**
 * 统计空间
 * @param period 统计周期:天
 * @param count 统计窗口
 */
case class StatisticsDuration(
    period: Int = month * 1,
    count: Int = 12 * 2
)
