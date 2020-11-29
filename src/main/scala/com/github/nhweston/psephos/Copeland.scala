package com.github.nhweston.psephos

import com.github.nhweston.psephos.util.Tally
import com.github.nhweston.psephos.util.Tally._

import scala.annotation.tailrec

object Copeland {

  case class Result[C](
    elected: Seq[C],
    tally: Tally[(C, C)],
    wins: Tally[C],
    losses: Tally[C],
    scores: Tally[C],
  )

  def apply[C](
    candidates: Seq[C],
    ballots: Seq[Seq[C]],
    numSeats: Int,
  ): Result[C] = {

    val tally = {
      @tailrec
      def aux(
        tally: Tally[(C, C)],
        ballot: Seq[C],
        lower: Set[C] = candidates.toSet,
      ): Tally[(C, C)] =
        ballot match {
          case c0 +: tl =>
            val next = lower.foldLeft(tally)((t, c1) => t.increment((c0, c1)))
            aux(next, tl, lower - c0)
          case Nil => tally
        }
      val pairs = for (c0 <- candidates; c1 <- candidates) yield (c0, c1)
      ballots.foldLeft(Tally(pairs))(aux(_, _))
    }

    val (wins, losses) = {
      @tailrec
      def aux(
        candidates: Seq[C] = candidates,
        wins: Tally[C] = Tally(candidates),
        losses: Tally[C] = Tally(candidates),
      ): (Tally[C], Tally[C]) =
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

    val scores = candidates.map(c => c -> (wins(c) - losses(c))).toMap
    val elected = candidates.sortBy(scores).takeRight(numSeats)
    Result(elected, tally, wins, losses, scores)

  }

}
