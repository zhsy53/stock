package com.cat.zsy.domain

case class StockElement(date: Int, openingPrice: Int, highestPrice: Int, lowestPrice: Int, closingPrice: Int, turnover: Float, volume: Int) {
  override def toString: String = s"日期:$date 开盘:$openingPrice 最高:$highestPrice 最低:$lowestPrice 收盘:$closingPrice 成交额:$turnover 成交量:$volume"
}
