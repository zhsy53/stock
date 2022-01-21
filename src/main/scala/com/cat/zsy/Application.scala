package com.cat.zsy

import com.cat.zsy.cache.DataFactory
import com.cat.zsy.domain.TongStockElement
import com.cat.zsy.strategy.TongStockPriceIndicator
import com.cat.zsy.strategy.config.PriceStrategyOption
import com.cat.zsy.strategy.executor.{DataHandler, PriceExecutor, PriceIndicatorFilter}
import org.slf4j.LoggerFactory

object Application extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  private val begin = System.currentTimeMillis()

  private val h: DataHandler[Seq[TongStockElement]] = seq => seq.takeRight(22 * 12 * 2).take(22 * 12)

  private val codes = "300103;300125;300200;300718;002123;300117;601872;".split(";").distinct.sorted

  private val option: PriceStrategyOption = PriceStrategyOption(maxPrice = Option(35))

  private val f = new PriceIndicatorFilter(option)

  val list = PriceExecutor.handleByElementsAndFilter(h = Option(h), f = Option(f))
  list.map(o => (o.code.substring(2), TongStockPriceIndicator(o.data))).foreach(o => log.info("{}\t{}\t{}", o._1, DataFactory.getName(o._1), o._2.toString))

//  val minProfitRatio = Option(5.0)
//
//  val order1: Ordering[TongStockPriceIndicator] = Ordering.by(-_.avgAmplitude)
//  val amplitudeList = change(order1).toSeq.filter(_._2 >= 6).sortWith((a, b) => a._2 > b._2).map(_._1)
//  log.info("共{}只\n", amplitudeList.mkString(";"))
//
//  val order2: Ordering[TongStockPriceIndicator] = Ordering.by(_.avgVariance)
//  val varianceList = change(order2).toSeq.filter(_._2 >= 6).sortWith((a, b) => a._2 > b._2).map(_._1)
//  log.info("共{}只\n", varianceList.mkString(";"))
//
//  val map = amplitudeList.zip(Range(1, 1000)).toMap
//  val list = varianceList
//    .zip(Range(1, 1000))
//    .map { case (code, index) => (code, index, map.getOrElse(code, -1)) }
//    .filter(_._3 > 0)
//  list.foreach(o => log.info("{}\t{}\t均值排名:{}\t振幅排名:{}", o._1, DataFactory.getName(o._1.substring(2)), o._2, o._3))

  //  val list =
//    "601118;002593;002031;000010;002295;002421;601616;600302;600839;600653;600249;300249;300021;002492;000637;002225;600467;000517;002780;600370;002380;000850;000430;002678;600737;000680;000997;600679;600560;600235;002495;600879;002186;002519;002296;300066;600299;600724;002823;002166;601901;000677;603808;000702;601555;002361;300232;002134;601128;300214;000712;601996;300321;600283;603029;000721;601099;600719;000909;600343;002698;002181;000096;601881;000557;600562;600784;000729;600026;603003;002066;002632;300231;600822;300150;300599;600883;300177;000020;300245;000880;300050;300188;300371;300539;300330;600865;002062;300380;002693;600300;002786;600785;603577;600768;002489;000886;000985;002795;600830;300555;002666;300295;002207;000570;603999;002660;600073"

  //  StrategyExecutor.executeForCurrent(dataFilter = o => list.contains(o.code.substring(2)), always = true)

//  StrategyExecutor.executeForCurrent(dataFilter = o => list.contains(o.code.substring(2)), strategyFilter = _ => true, always = true)
//    .filter(o => o.stock.data.findLast(p => p.closingPrice <= o.avgPrice * (100 - 5)).map(_.date).exists(d => d >= 20211201))
//    .foreach(o => log.info(o.toString()))

  //  Range(d + 30, d, -1).foreach(t => executeForSimulation(backtracking = t, observation = d, detail = true))

  val end = System.currentTimeMillis()

  log.warn("cost " + (end - begin) + " mills")

//  private def change(order: Ordering[TongStockPriceIndicator]): mutable.Map[String, Int] = {
//    def dh(duration: StatisticsDuration) = new TruncateDataHandler(duration.period * duration.count)
//    def df(duration: StatisticsDuration, o: PriceStrategyOption): DataFilter[TongStockHistory] = history => new PriceIndicatorFilter(o).filter(TongStockPriceIndicator(history, duration))
//
//    def countByCode(seq: Seq[(StatisticsDuration, PriceStrategyOption)]): mutable.Map[String, Int] = seq.par
//      .map(o => PriceExecutor.handleAndFilter(Option(dh(o._1)), Option(df(o._2, o._1))))
//      .map(o => o.sorted(order))
//      .flatMap(o => o.map(_.code))
//      .foldLeft(mutable.HashMap.empty[String, Int].withDefaultValue(0))((acc, x) => { acc(x) = acc(x) + 1; acc })
//
//    val perMonth = List(1, 2, 3)
//    val monthCounts = List(12, 18, 24, 30, 36, 42)
//
//    val config = monthCounts
//      .map(c => perMonth.map(m => (c, m)))
//      .flatMap(o => o.map { case (c, m) => (StatisticsDuration(month * m, 12 * c / m), PriceStrategyOption(count = Option(month * c))) })
//
//    countByCode(config)
//  }

//  private def counts[T](xs: IterableOnce[T]): Map[T, Int] = {
//    xs.iterator.foldLeft(Map.empty[T, Int].withDefaultValue(0))((acc, x) => { acc(x) += 1; acc })
//  }

//  private def countByCode(d: StatisticsDuration, o: PriceStrategyOption) = {
//    val dh = new TruncateDataHandler(d.period * d.count);
//    def df(o: PriceStrategyOption, mapper: TongStockHistory => TongStockPriceIndicator): DataFilter[TongStockHistory] = history => new PriceIndicatorFilter(o).filter(mapper(history))
//
//    val list = PriceExecutor.handle(Option(dh))
//    val indicators = list.map(TongStockPriceIndicator(_, d))
//    val map = indicators.map(o => (o.stock.code, o)).toMap
////    , Option(df(o._2))
//  }
}
