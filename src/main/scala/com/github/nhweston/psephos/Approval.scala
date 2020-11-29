package com.github.nhweston.psephos

import com.github.nhweston.psephos.util.Tally
import com.github.nhweston.psephos.util.Tally._

case class Approval[C](
  candidates: Seq[C],
  ballots: Seq[Seq[C]],
  numSeats: Int = 1,
) {

  lazy val tally: Map[C, Int] = ballots.flatten.foldLeft(Tally[C, Int](candidates))(_.increment(_))

  lazy val elected: Seq[C] = candidates.sortBy(tally(_)).takeRight(numSeats)

}
