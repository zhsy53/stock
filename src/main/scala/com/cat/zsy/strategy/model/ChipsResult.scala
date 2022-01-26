package com.cat.zsy.strategy.model

import com.cat.zsy.domain.TongStockElement

case class ChipsResult(min: TongStockElement, in: TongStockElement, loss: Seq[TongStockElement], profit: Seq[TongStockElement], highest: TongStockElement, lowest: TongStockElement) {
  override def toString: String = {
    f"${result}\t低点:[${min.date}](${min.lowestPrice / 100.0}%5.2f)\t入场:[${in.date}](${(in.highestPrice + in.lowestPrice) / 200.0}%5.2f)" +
      f"\t盈利:$profitResult%16s\t亏损:$lossResult%16s" +
      f"\t最高:[${highest.date}](${highest.highestPrice / 100.0}%5.2f)\t最低:[${lowest.date}](${lowest.lowestPrice / 100.0}%5.2f)"
  }

  def profitResult: String = if (isProfit) f"[${profit.head.date}](${profit.head.closingPrice / 100.0}%5.2f)" else ""

  def isProfit: Boolean = profit.nonEmpty && (loss.isEmpty || profit.head.date < loss.head.date)

  def result: String = if (isLoss) "-----" else if (isProfit) "+++++" else "*****"

  def isLoss: Boolean = loss.nonEmpty && (profit.isEmpty || loss.head.date < profit.head.date)

  def lossResult: String = if (isLoss) f"[${loss.head.date}](${loss.head.closingPrice / 100.0}%5.2f)" else ""

  def isUnknown: Boolean = loss.isEmpty && profit.isEmpty
}
