package com.cat.zsy.strategy

case class ChipsModel(
    code: String,
    name: String,
    avg: Int,
    lowestDate: Int,
    lowestPrice: Int,
    highestDate: Int,
    highestPrice: Int
) {
  def isST: Boolean = name.contains("ST")
  def increase: Double = highestPrice * 100.0 / lowestPrice - 100
  override def toString: String = f"$code\t$name\t${avg / 100.0}%5.2f\t[$lowestDate](${lowestPrice / 100.0}%5.2f) -> [$highestDate](${highestPrice / 100.0}%5.2f) $increase%.2f${"%"}"
}
