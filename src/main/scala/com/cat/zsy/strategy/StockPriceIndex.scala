package com.cat.zsy.strategy

/**
 * @deprecated
 */
object StockPriceIndex {
  def avg(list: Seq[BigDecimal]): BigDecimal = list.sum / list.size
  def max(list: Seq[BigDecimal]): BigDecimal = list.max
  def min(list: Seq[BigDecimal]): BigDecimal = list.min
  def ma5(list: Seq[BigDecimal]): Seq[BigDecimal] = movingSum(list, 5)

  /**
   * @param seq 列表
   * @param period 滑动窗口
   * @return 移动均值
   */
  def movingSum(seq: Seq[BigDecimal], period: Int): Seq[BigDecimal] = period match {
    case x if x < 1 => throw new IllegalArgumentException
    case 1          => seq
    case _          => seq.sliding(period).map(_.sum / period).toSeq
  }
}
