package com.cat.zsy.domain

case class StockSeq(code: String, data: Seq[StockElement]) {
  override def toString: String = s"""
                                     |[编码]:$code
                                     |data.mkString("\n")
                                     |""".stripMargin
}
