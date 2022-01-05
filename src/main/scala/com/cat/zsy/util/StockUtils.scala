package com.cat.zsy.util

object StockUtils {
  def fixCode(code: String): String = if (code.length == 8) code else if (code.startsWith("6")) "sh" + code else "sz" + code

  def intToDecimal(i: Int): BigDecimal = (BigDecimal(i) / 100).setScale(2)
}
