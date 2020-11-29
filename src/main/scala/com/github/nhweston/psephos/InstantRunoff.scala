package com.github.nhweston.psephos

import scala.annotation.tailrec

case class InstantRunoff[C](
  candidates: Seq[C],
  ballots: Seq[Seq[C]],
  numSeats: Int,
) {

  sealed trait Determination
  case class Eliminate(candidate: C) extends Determination
  case class Elect(candidate: C) extends Determination
  case object ElectAll extends Determination

  case class Votes(
    ballots: Seq[Ballot],
    value: Double,
  ) {
    def +(ballot: Ballot): Votes = Votes(ballots :+ ballot, value + ballot.value)
  }

  case class Ballot(
    preferences: Seq[C],
    value: Double,
  )

  lazy val quota: Double = candidates.size / (numSeats + 1)

  lazy val count: Count = {
    val tally = ballots.filter(_.nonEmpty).groupMap(_.head)(Ballot(_, 1.0))
      .view.mapValues(ballots => Votes(ballots, ballots.size.toDouble)).toMap
    Count(tally, candidates.toSet, Set.empty, Set.empty)
  }

  lazy val elected: Seq[C] = {
    @tailrec
    def aux(
      result: Seq[C] = Seq.empty,
      count: Count = count,
    ): Seq[C] = {
      val nextResult =
        result ++ (count.determination match {
          case Eliminate(_) => Seq.empty
          case Elect(candidate) => Seq(candidate)
          case ElectAll => count.remaining.toSeq.sortBy(count.tally(_).value).reverse
        })
      count.nextCount match {
        case Some(nextCount) => aux(nextResult, nextCount)
        case None => nextResult
      }
    }
    aux()
  }

  case class Count(
    tally: Map[C, Votes],
    remaining: Set[C],
    elected: Set[C],
    eliminated: Set[C],
  ) {

    lazy val maxCandidate: C = remaining.maxBy(tally(_).value)
    lazy val minCandidate: C = remaining.minBy(tally(_).value)

    lazy val determination: Determination =
      if (numSeats <= remaining.size) ElectAll
      else if (tally(maxCandidate).value > quota) Elect(maxCandidate)
      else Eliminate(minCandidate)

    lazy val nextCount: Option[Count] =
      determination match {
        case Eliminate(c) =>
          val tallyNext =
            tally(c).ballots.foldLeft(tally - c) { case (tally, Ballot(prefs, value)) =>
              val prefsNext = prefs.dropWhile(!remaining(_))
              if (prefsNext.nonEmpty) {
                val ballotNext = Ballot(prefsNext, value)
                tally.updatedWith(c)(_.map(_ + ballotNext))
              }
              else tally
            }
          Some(Count(tallyNext, remaining - c, elected, eliminated + c))
        case Elect(c) =>
          if (numSeats < elected.size + 1) None
          else {
            val tallyNext = {
              val scale = (tally(c).value - quota) / quota
              tally(c).ballots.foldLeft(tally + (c -> Votes(Seq.empty, quota))) {
                case (tally, Ballot(prefs, value)) =>
                  val prefsNext = prefs.dropWhile(!remaining(_))
                  if (prefsNext.nonEmpty) {
                    val ballotNext = Ballot(prefsNext, value * scale)
                    tally.updatedWith(c)(_.map(_ + ballotNext))
                  }
                  else tally
              }
            }
            Some(Count(tallyNext, remaining - c, elected + c, eliminated))
          }
        case ElectAll => None
      }

  }

}
