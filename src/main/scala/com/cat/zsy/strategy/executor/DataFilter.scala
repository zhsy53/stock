package com.cat.zsy.strategy.executor

import com.cat.zsy.domain.TongStockHistory
import com.cat.zsy.strategy.TongStockPriceIndicator
import com.cat.zsy.strategy.config.PriceStrategyOption

import scala.collection.mutable.ListBuffer

trait DataFilter[T] { def filter(t: T): Boolean }

object DataFilter { def compose[T](fs: Seq[DataFilter[T]]): DataFilter[T] = t => fs.forall(h => h.filter(t)) }

class CodeFilter(o: Seq[String]) extends DataFilter[TongStockHistory] { override def filter(t: TongStockHistory): Boolean = o.contains(t.code.substring(2)) }

class EnoughFilter(o: Int) extends DataFilter[TongStockHistory] { override def filter(t: TongStockHistory): Boolean = t.data.size >= o }

class MaxPriceFilter(o: Double) extends DataFilter[TongStockHistory] { override def filter(t: TongStockHistory): Boolean = TongStockPriceIndicator(t.data).avgPrice <= o }

class MinAmplitudeFilter(o: Double) extends DataFilter[TongStockHistory] { override def filter(t: TongStockHistory): Boolean = TongStockPriceIndicator(t.data).avgAmplitude >= o }

class MaxAvgVarianceFilter(o: Double) extends DataFilter[TongStockHistory] { override def filter(t: TongStockHistory): Boolean = TongStockPriceIndicator(t.data).avgVariance <= o }

class PriceIndicatorFilter(o: PriceStrategyOption) extends DataFilter[TongStockHistory] {
  override def filter(t: TongStockHistory): Boolean = {
    val buffer = ListBuffer[DataFilter[TongStockHistory]]()

    o.codes.map(o => new CodeFilter(o)).foreach(buffer += _)
    o.count.map(o => new EnoughFilter(o)).foreach(buffer += _)
    o.maxPrice.map(o => new MaxPriceFilter(o)).foreach(buffer += _)
    o.minAmplitude.map(o => new MinAmplitudeFilter(o)).foreach(buffer += _)
    o.maxAvgVariance.map(o => new MaxAvgVarianceFilter(o)).foreach(buffer += _)

    DataFilter.compose(buffer.toList).filter(t)
  }
}
