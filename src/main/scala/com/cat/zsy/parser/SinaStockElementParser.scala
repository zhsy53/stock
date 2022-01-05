package com.cat.zsy.parser
import com.cat.zsy.domain.SinaStockElement

import java.time.{LocalDate, LocalTime}

object SinaStockElementParser {
  private val resultPrefix = "var hq_str_"

  def parse(s: String): SinaStockElement = {
    val arr = s.substring(s.indexOf("\"") + 1, s.lastIndexOf("\"")).split(",")

    SinaStockElement(
      s.substring(resultPrefix.length, resultPrefix.length + 8),
      arr(0),
      LocalDate.parse(arr(30)),
      LocalTime.parse(arr(31)),
      BigDecimal(arr(1)),
      BigDecimal(arr(2)),
      BigDecimal(arr(3)),
      BigDecimal(arr(4)),
      BigDecimal(arr(5))
    )
  }
}
