package com.cat.zsy.parser

import com.cat.zsy.domain.TongStockElement

import java.nio.ByteBuffer

object TongStockElementParser {
  def parse(rows: Array[Byte]): Seq[TongStockElement] = rows.grouped(32).map(parseInternal).toSeq

  private def parseInternal(row: Array[Byte]): TongStockElement = {
    def bytesToInt(bs: Array[Byte]): Int = ByteBuffer.wrap(bs.reverse).getInt()
    def bytesToFloat(bs: Array[Byte]): Float = ByteBuffer.wrap(bs.reverse).getFloat()

    val cols = row.grouped(4).map(bytesToInt).toArray

    TongStockElement(cols(0), cols(1), cols(2), cols(3), cols(4), bytesToFloat(row.slice(20, 24)), cols(6))
  }
}
