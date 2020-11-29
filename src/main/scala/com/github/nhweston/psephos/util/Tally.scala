package com.github.nhweston.psephos.util

object Tally {

  implicit class MapImplicits[K, V](self: Map[K, V])(implicit num: Numeric[V]) {
    def increment(key: K): Map[K, V] = self.updatedWith(key)(_.map(num.plus(_, num.one)))
  }

  def apply[K, V](keys: Seq[K])(implicit num: Numeric[V]): Map[K, V] =
    keys.map(_ -> num.zero).toMap

}
