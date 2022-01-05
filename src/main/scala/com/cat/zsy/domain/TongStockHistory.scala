package com.cat.zsy.domain

case class TongStockHistory(code: String, data: Seq[TongStockElement]) {
  override def toString: String = s"""
                                     |[编码]:$code
                                     |${data.mkString("\n")}
                                     |""".stripMargin
}
