package com.cat.zsy.util

import scala.Numeric.Implicits._
import scala.collection.mutable
import scala.language.implicitConversions

object MathUtils {
  val month = 22

  def fixCode(code: String): String = if (code.length == 8) code else if (code.startsWith("6")) "sh" + code else "sz" + code

  def formatArray(it: Iterable[Any]): String = it.mkString("[", ", ", "]")

  def percentageToDouble(i: Int): Double = i.toDouble / 100

  // 方差
  def variance[T: Numeric](it: Iterable[T]): Double = {
    val _avg = avg(it)

    math.sqrt(it.map(_.toDouble).map(o => math.pow(o - _avg, 2)).sum) / (it.size * _avg)
  }

  // 均值
  def avg[T: Numeric](it: Iterable[T]): Double = it.sum.toDouble / it.size

  def divPercentage(a: Int, b: Int): Double = if (b == 0) 100 else a * 100.0 / b

  def between(d: Double, d1: Double, d2: Double): Boolean = d >= d1 && d <= d2

  def increase(seq: Seq[Double]): Seq[Double] = Range(1, seq.size).map(i => seq(i) / seq(i - 1)).map(_ - 1)

  def counts[T](xs: IterableOnce[T]): Map[T, Int] = {
    xs.iterator.foldLeft(mutable.Map.empty[T, Int].withDefaultValue(0))((acc, x) => { acc(x) += 1; acc }).toMap
  }
}
