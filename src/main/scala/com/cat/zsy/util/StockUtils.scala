package com.cat.zsy.util

import scala.Numeric.Implicits._

object StockUtils {
  def fixCode(code: String): String = if (code.length == 8) code else if (code.startsWith("6")) "sh" + code else "sz" + code

  def intToDecimal(i: Int): BigDecimal = (BigDecimal(i) / 100).setScale(2)

  def main(args: Array[String]): Unit = {
    stdDev(List(1, 2, 3, 4))
  }

  def stdDev[T: Numeric](xs: Iterable[T]): Double = math.sqrt(variance(xs))

  def variance[T: Numeric](xs: Iterable[T]): Double = {
    val avg = mean(xs)
    xs.map(_.toDouble).map(a => math.pow(a - avg, 2)).sum / xs.size

  }

  def mean[T: Numeric](xs: Iterable[T]): Double = xs.sum.toDouble / xs.size
}
