package com.cat.zsy.domain
import java.time.{LocalDate, LocalTime}

case class SinaStockElement(
    code: String,
    name: String,
    date: LocalDate,
    time: LocalTime,
    openingPrice: BigDecimal,
    yesterdayClosingPrice: BigDecimal,
    currentPrice: BigDecimal,
    highestPrice: BigDecimal,
    lowestPrice: BigDecimal
)
