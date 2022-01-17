package com.cat.zsy.strategy

import com.cat.zsy.util.StockUtils._

/**
 * 统计空间
 * @param period 统计周期:天
 * @param count 统计窗口
 */
case class StrategyDuration(
    period: Int = month * 3,
    count: Int = 8
)
