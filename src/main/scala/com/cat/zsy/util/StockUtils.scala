package com.cat.zsy.util

import scala.Numeric.Implicits._

object StockUtils {
  def fixCode(code: String): String = if (code.length == 8) code else if (code.startsWith("6")) "sh" + code else "sz" + code

  def formatArray(it: Iterable[Any]): String = it.mkString("[", ", ", "]")

  def percentageToDecimal(i: Int): Double = i.toDouble / 100

  // 方差
  def variance[T: Numeric](it: Iterable[T]): Double = {
    val _avg = avg(it)

    math.sqrt(it.map(_.toDouble).map(o => math.pow(o - _avg, 2)).sum) / (it.size * _avg)
  }

  // 均值
  def avg[T: Numeric](it: Iterable[T]): Double = it.sum.toDouble / it.size
}
