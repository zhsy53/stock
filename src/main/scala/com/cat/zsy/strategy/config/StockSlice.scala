package com.cat.zsy.strategy.config

/**
 * @param offset 从后 -> 取最近的数据
 */
case class StockSlice(offset: Int, length: Int) {}

object StockSlice { def apply(length: Int): StockSlice = StockSlice(0, length) }
