package com.cat.zsy.api
import com.cat.zsy.domain.SinaStockElement
import com.cat.zsy.parser.SinaStockElementParser
import com.cat.zsy.util.StockUtils
import scalaj.http.{Http, HttpOptions}

object SinaQuotesApi {
  private val url = "http://hq.sinajs.cn/list="

  def getData(codes: Seq[String]): Seq[SinaStockElement] = {
    Http(url + codes.map(StockUtils.fixCode).mkString(","))
      .header("Charset", "UTF-8")
      .option(HttpOptions.readTimeout(10000))
      .asString
      .body
      .split("\n")
      .map(SinaStockElementParser.parse)
  }
}
