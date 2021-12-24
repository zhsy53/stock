package com.cat.zsy.parser

import com.cat.zsy.domain.StockElement

import java.nio.ByteBuffer

/**
 * https://www.ilovematlab.cn/thread-226577-1-1.html
 */
object StockElementParser {
  def parseDates(rows: Array[Byte]): Seq[StockElement] = rows.grouped(32).map(parseDate).toSeq

  private def parseDate(row: Array[Byte]): StockElement = {
    def bytesToInt(bs: Array[Byte]): Int = ByteBuffer.wrap(bs.reverse).getInt()
    def bytesToFloat(bs: Array[Byte]): Float = ByteBuffer.wrap(bs.reverse).getFloat()

    val cols = row.grouped(4).map(bytesToInt).toArray

    StockElement(cols(0), cols(1), cols(2), cols(3), cols(4), bytesToFloat(row.slice(20, 24)), cols(6))
  }
}
