package com.cat.zsy.util
import scala.collection.mutable.ListBuffer

object OscillationCalculator {
  def oscillation(list: Seq[Int], base: Int): Seq[Int] = {
    val r = ListBuffer[Int]()

    val filter = list.filter(_ != base)

    r += { if (filter.head > base) 1 else -1 }

    filter.tail.map(_ > base).foreach(o => if (o ^ r.last > 0) r += boolToSign(o) else r.update(r.size - 1, r.last + boolToSign(o)))

    r.toSeq
  }

  private def boolToSign: Boolean => Int = o => if (o) 1 else -1
}
