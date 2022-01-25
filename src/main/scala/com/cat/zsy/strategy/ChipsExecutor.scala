package com.cat.zsy.strategy
import com.cat.zsy.domain.TongStockElement
import org.slf4j.LoggerFactory

object ChipsExecutor {
  private val log = LoggerFactory.getLogger(this.getClass)
  private val d = 7 // 假设整个行情的最短周期,同时在底部的徘徊周期也大于此值
  private val ratio = 0.3

  case class MinMax(min: TongStockElement, max: TongStockElement)

  private def minAndMax(seq: Seq[TongStockElement]): MinMax = MinMax(seq.minBy(_.lowestPrice), seq.maxBy(_.highestPrice))

  private def validate(mm: MinMax, segment: Seq[TongStockElement]): Boolean = {
    val later = minAndMax(segment)
    later.max.highestPrice <= mm.max.highestPrice && later.min.lowestPrice >= mm.min.lowestPrice
  }

//  private def search(seq: Seq[TongStockElement]): Seq[MinMax] = {
//    val init = minAndMax(Seq(seq.head))
//
//  }

  def main(args: Array[String]): Unit = {}
}
