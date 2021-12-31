package com.cat.zsy.strategy

import com.cat.zsy.domain.StockSeq
import com.cat.zsy.strategy.StrategyFactory.log
import org.slf4j.LoggerFactory

trait StockStrategy {
  def select(seq: StockSeq): Boolean
}

class IntervalStrategy(days: Int) extends StockStrategy {
  private val log = LoggerFactory.getLogger(this.getClass)

  override def select(seq: StockSeq): Boolean = {
    val list = seq.data

    val r = list.size >= days
    if (!r) log.trace("{} => 数据太少,不支持待统计区间", seq.code)
    r
  }
}

class PriceStrategy(price: Int) extends StockStrategy {
  override def select(seq: StockSeq): Boolean = {
    val current = seq.data.last.highestPrice
    BigDecimal(current) / 100 <= price
  }
}

class ImmediateStrategy extends StockStrategy {
  override def select(seq: StockSeq): Boolean = { false }
}
