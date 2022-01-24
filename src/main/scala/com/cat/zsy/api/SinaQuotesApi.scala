package com.cat.zsy.api
import com.cat.zsy.domain.SinaStockElement
import com.cat.zsy.parser.SinaStockElementParser
import com.cat.zsy.util.MathUtils
import scalaj.http._

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object SinaQuotesApi {
  private val url = "http://hq.sinajs.cn/list="

  def getData(codes: Seq[String]): Seq[SinaStockElement] = {
    codes.grouped(200).toList.par.flatMap(_getData).toList
  }

  private def _getData(codes: Seq[String]): Seq[SinaStockElement] = {
    Http(url + codes.map(MathUtils.fixCode).mkString(","))
      .header("Charset", "UTF-8")
      .option(HttpOptions.readTimeout(10000))
      .asString
      .body
      .split("\n")
      .map(SinaStockElementParser.parse)
  }
}
