package com.cat.zsy.util

import scala.Numeric.Implicits._
import scala.math.BigDecimal.RoundingMode.HALF_UP

object StockUtils {
  def fixCode(code: String): String = if (code.length == 8) code else if (code.startsWith("6")) "sh" + code else "sz" + code

  def formatArray(it: Iterable[Any]): String = it.mkString("[", ", ", "]")

  def percentageToDecimal(i: Int): BigDecimal = (BigDecimal(i) / 100).setScale(2, HALF_UP)

  // 方差
  def variance[T: Numeric](it: Iterable[T]): BigDecimal = {
    val _avg = avg(it).toDouble

    // 差的平方
    def difSquare: Double = it.map(_.toDouble).map(o => math.pow(o - _avg, 2) * 1000000).sum

    (BigDecimal.valueOf(math.sqrt(difSquare)) / (it.size * _avg)).setScale(2, HALF_UP)
  }

  // 均值
  def avg[T: Numeric](it: Iterable[T]): BigDecimal = (BigDecimal.valueOf(it.sum.toDouble) / it.size).setScale(2, HALF_UP)
}
