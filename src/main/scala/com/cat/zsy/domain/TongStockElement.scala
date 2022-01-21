package com.cat.zsy.domain

case class TongStockElement(date: Int, openingPrice: Int, highestPrice: Int, lowestPrice: Int, closingPrice: Int, turnover: Float, volume: Int) {
  override def toString: String = s"日期:$date 开盘:$openingPrice 最高:$highestPrice 最低:$lowestPrice 收盘:$closingPrice 成交额:$turnover 成交量:$volume"

  def increase: Double = closingPrice.toDouble / openingPrice - 1

  def perIncreaseVolume: Double = volume / increase
}
