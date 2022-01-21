package com.cat.zsy.strategy.config

/**
 * @param offset 从后 -> 取最近的数据
 */
case class StockSlice(offset: Int = 0, length: Int)
