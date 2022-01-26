package com.cat.zsy.strategy.executor

import com.cat.zsy.domain.{TongStockElement, TongStockHistory}
import com.cat.zsy.strategy.config.StockSlice
import com.cat.zsy.strategy.model.ChipsResult
import com.cat.zsy.util.MathUtils.month

object ChipsExecutor {
  // 极值邻域(两侧)天数
  private val neighborhood = 7

  // 入场后观察天数
  private val observeDays = month * 3

  // 极值参考区间
  private val referDays = observeDays * 3

  // 模拟区间
  private val slice: StockSlice = StockSlice(observeDays + 1, month * 12)

  def run(t: TongStockHistory, neighborhood: Int = neighborhood, slice: StockSlice = slice): Seq[ChipsResult] = {
    findMinimumWithRefer(t, neighborhood, slice).map(o => check(o, t.data)).filter(_.isDefined).map(_.get)
  }

  def findMinimumWithRefer(t: TongStockHistory, neighborhood: Int = neighborhood, slice: StockSlice = slice): Seq[TongStockElement] = {
    // 统计区间不够
    if (t.data.size <= slice.offset + slice.length + referDays) return Seq.empty

    val target = t.data.takeRight(slice.offset + slice.length).take(slice.length)

    val refer = t.data.takeRight(slice.offset + slice.length + referDays).take(referDays)

    val highest = refer.maxBy(_.highestPrice).highestPrice
    val avg = refer.map(_.closingPrice).sum / refer.size
    val lowest = refer.minBy(_.lowestPrice).lowestPrice
    println(t.code + ":" + highest + "\t" + avg + "\t" + lowest)

    // TODO:待配置
    findMinimum(target, neighborhood)
      .filter(o => highest.toDouble / o.lowestPrice >= 1.3)
  }

  def findMinimum(seq: Seq[TongStockElement], neighborhood: Int): Seq[TongStockElement] = {
    val size = seq.size
    if (size <= neighborhood * 2) return Seq.empty

    Range(0, size)
      .zip(seq)
      .filter { case (i, e) => (i >= neighborhood || i <= size - neighborhood) && le(e, bothSidesData(seq, i, neighborhood)) }
      .map { case (_, e) => e }
  }

  private def le(t: TongStockElement, list: Seq[TongStockElement]): Boolean = list.forall(_.lowestPrice >= t.lowestPrice)

  private def bothSidesData[T](seq: Seq[T], index: Int, neighborhood: Int = 7): Seq[T] = seq.slice(index - neighborhood, index) ++ seq.slice(index + 1, index + 1 + neighborhood)

  private def check(min: TongStockElement, seq: Seq[TongStockElement]): Option[ChipsResult] = {
    val after = seq.filter(_.date > min.date).take(neighborhood + 1)

//    if (after.count(o => o.closingPrice > o.openingPrice) <= 1) return Option.empty

    val in = after.last

    val observe = seq.filter(_.date > in.date).take(observeDays)

    val buyPrice = (in.highestPrice + in.lowestPrice) / 2

    if (buyPrice >= min.lowestPrice * 1.06) return Option.empty

    Option(
      ChipsResult(
        min,
        in,
        observe.filter(_.lowestPrice <= min.lowestPrice / 1.15),
        observe.filter(o => (o.highestPrice + o.lowestPrice) / 2 >= buyPrice * 1.25),
        observe.maxBy(_.closingPrice),
        observe.minBy(_.closingPrice)
      )
    )
  }

  def findMaximum(seq: Seq[TongStockElement], neighborhood: Int = neighborhood): Seq[TongStockElement] = {
    val size = seq.size
    if (size <= neighborhood * 2) return Seq.empty

    Range(0, size)
      .zip(seq)
      .filter { case (i, e) => (i >= neighborhood || i <= size - neighborhood) && ge(e, bothSidesData(seq, i, neighborhood)) }
      .map { case (_, e) => e }
  }

  private def ge(t: TongStockElement, list: Seq[TongStockElement]): Boolean = list.forall(_.highestPrice <= t.highestPrice)

  private def extremum(t: TongStockElement, list: Seq[TongStockElement]): Boolean = le(t, list) || ge(t, list)
}
