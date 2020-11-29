package com.github.nhweston.psephos.util

object Tally {

  type Tally[C] = Map[C, Int]

  implicit class TallyImplicits[C](self: Tally[C]) {
    def increment(candidate: C): Tally[C] =
      self.updatedWith(candidate)(_.map(_ + 1))
  }

  def apply[C](candidates: Seq[C]): Tally[C] =
    candidates.map(_ -> 0).toMap

}
