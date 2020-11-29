package com.github.nhweston.psephos

import com.github.nhweston.psephos.util.Tally
import com.github.nhweston.psephos.util.Tally._

object Approval {

  case class Result[C](
    elected: Seq[C],
    tally: Tally[C],
  )

  def apply[C](
    candidates: Seq[C],
    ballots: Seq[Seq[C]],
    numSeats: Int = 1,
  ): Result[C] = {
    val tally = ballots.flatten.foldLeft(Tally(candidates))(_.increment(_))
    val elected = candidates.sortBy(tally(_)).takeRight(numSeats)
    Result(elected, tally)
  }

}
