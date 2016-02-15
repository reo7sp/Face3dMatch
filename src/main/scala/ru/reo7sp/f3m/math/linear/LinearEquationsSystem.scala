/*
 * Copyright 2016 Oleg Morozenkov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ru.reo7sp.f3m.math.linear

import ru.reo7sp.f3m.math.linear.LinearEquationsSystem.SolutionCount

case class LinearEquationsSystem(A: Matrix[Double], x: Seq[Var[Double]], b: Seq[Double]) {
  require(A.hasOnlyConsts)
  require(A.width == x.size)
  require(A.height == b.size)

  def solve: (Seq[Var[Double]], SolutionCount.Value) = {
    // http://e-maxx.ru/algo/linear_systems_gauss
    val arr = A.toMultidimensionalArray.map(_.map(_.right.get))

    val where = Array.fill(A.width)(-1)
    for (row <- 0 until A.height) {
      var done = false
      for (col <- 0 until A.width if !done) {
        var sel = row
        for (i <- row until A.height if arr(i)(col).abs > arr(sel)(col).abs) {
          sel = i
        }
        if (arr(sel)(col) != 0) {
          val t = arr(sel)
          arr(sel) = arr(row)
          arr(row) = t

          where(col) = row
          done = true

          for (i <- 0 until A.height if i != row) {
            val c = arr(i)(col) / arr(row)(col)
            for (j <- col to A.width) {
              arr(i)(j) -= arr(row)(j) * c
            }
          }
        }
      }
    }

    val ans = Array.fill(A.width)(0.0)

    for (i <- 0 until A.width if where(i) != -1) {
      ans(i) = arr(where(i))(A.width) / arr(where(i))(i)
    }

    for (i <- 0 until A.height) {
      var sum = 0.0
      for (j <- 0 until A.width) {
        sum += ans(j) * arr(i)(j)
      }
      if (sum - arr(i)(A.width - 1) != 0) {
        return (Seq[Var[Double]](), SolutionCount.Zero)
      }
    }

    ((x zip ans).map { case (variable, value) => variable.copy(value = Some(value)) },
      if (where.contains(-1)) SolutionCount.Infinity else SolutionCount.One)
  }
}

object LinearEquationsSystem {

  object SolutionCount extends Enumeration {
    val Zero, One, Infinity = Value
  }

}
