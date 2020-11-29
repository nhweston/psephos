package com.github.nhweston.psephos

import com.github.nhweston.psephos.util.Tally
import com.github.nhweston.psephos.util.Tally._

import scala.annotation.tailrec

case class Copeland[C](
  candidates: Seq[C],
  ballots: Seq[Seq[C]],
  numSeats: Int = 1,
) {

  /**
   * Maps each pair of candidates to the number votes expressing preference for the first candidate
   * over the second.
   */
  lazy val tally: Map[(C, C), Int] = {
    @tailrec
    def aux(
      tally: Map[(C, C), Int],
      ballot: Seq[C],
      lower: Set[C] = candidates.toSet,
    ): Map[(C, C), Int] =
      ballot match {
        case c0 +: tl =>
          val next = lower.foldLeft(tally)((t, c1) => t.increment((c0, c1)))
          aux(next, tl, lower - c0)
        case Nil => tally
      }
    val pairs = for (c0 <- candidates; c1 <- candidates) yield (c0, c1)
    ballots.foldLeft(Tally[(C, C), Int](pairs))(aux(_, _))
  }

  lazy val (wins, losses): (Map[C, Int], Map[C, Int]) = {
    @tailrec
    def aux(
      candidates: Seq[C] = candidates,
      wins: Map[C, Int] = Tally(candidates),
      losses: Map[C, Int] = Tally(candidates),
    ): (Map[C, Int], Map[C, Int]) =
      candidates match {
        case c0 +: tl =>
          val (winsNext, lossesNext) =
            tl.foldLeft((wins, losses)) { case ((w, l), c1) =>
              val votes0 = tally((c0, c1))
              val votes1 = tally((c1, c0))
              if (votes0 > votes1)
                (w.increment(c0), l.increment(c1))
              else if (votes1 > votes0)
                (w.increment(c1), l.increment(c0))
              else (w, l)
            }
          aux(tl, winsNext, lossesNext)
        case Nil => (wins, losses)
      }
    aux()
  }

  lazy val scores: Map[C, Int] = candidates.map(c => c -> (wins(c) - losses(c))).toMap

  lazy val elected: Seq[C] = candidates.sortBy(scores).takeRight(numSeats)

}
