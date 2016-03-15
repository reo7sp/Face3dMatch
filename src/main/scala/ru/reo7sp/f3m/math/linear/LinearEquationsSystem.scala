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

import ru.reo7sp.f3m.math.linear.LinearEquationsSystem.{GaussSolver, LinearEquationsSystemSolver, SolutionCount}

case class LinearEquationsSystem(A: Matrix[Double], x: Seq[Var[Double]], b: Seq[Double]) {
  require(A.hasOnlyConsts)
  require(A.width == x.size)
  require(A.height == b.size)

  def solve(implicit solver: LinearEquationsSystemSolver = GaussSolver): (Seq[Var[Double]], SolutionCount.Value) = solver(this)
}

object LinearEquationsSystem {

  object SolutionCount extends Enumeration {
    val Zero, One, Infinity = Value
  }

  trait LinearEquationsSystemSolver {
    def apply(system: LinearEquationsSystem): (Seq[Var[Double]], SolutionCount.Value)
  }

  object GaussSolver extends LinearEquationsSystemSolver {
    override def apply(system: LinearEquationsSystem): (Seq[Var[Double]], SolutionCount.Value) = {
      // http://e-maxx.ru/algo/linear_systems_gauss
      val arr = system.A.toMultidimensionalArray.map(_.map(_.right.get))

      val where = Array.fill(system.A.width)(-1)
      for (row <- 0 until system.A.height) {
        var done = false
        for (col <- 0 until system.A.width if !done) {
          var sel = row
          for (i <- row until system.A.height if arr(i)(col).abs > arr(sel)(col).abs) {
            sel = i
          }
          if (arr(sel)(col) != 0) {
            val t = arr(sel)
            arr(sel) = arr(row)
            arr(row) = t

            where(col) = row
            done = true

            for (i <- 0 until system.A.height if i != row) {
              val c = arr(i)(col) / arr(row)(col)
              for (j <- col to system.A.width) {
                arr(i)(j) -= arr(row)(j) * c
              }
            }
          }
        }
      }

      val ans = Array.fill(system.A.width)(0.0)

      for (i <- 0 until system.A.width if where(i) != -1) {
        ans(i) = arr(where(i))(system.A.width) / arr(where(i))(i)
      }

      for (i <- 0 until system.A.height) {
        var sum = 0.0
        for (j <- 0 until system.A.width) {
          sum += ans(j) * arr(i)(j)
        }
        if (sum - arr(i)(system.A.width - 1) != 0) {
          return (Seq[Var[Double]](), SolutionCount.Zero)
        }
      }

      val result = (system.x zip ans).map { case (variable, value) => variable.copy(value = Some(value)) }
      val solutionCount = if (where.contains(-1)) SolutionCount.Infinity else SolutionCount.One
      (result, solutionCount)
    }
  }

  object KramerSolver extends LinearEquationsSystemSolver {
    override def apply(system: LinearEquationsSystem): (Seq[Var[Double]], SolutionCount.Value) = {
      val elems = system.A.elements.view

      def matrix(i: Int) = i match {
        case 0 =>
          elems.map(_.right.get)
        case _ =>
          elems.zipWithIndex.map { case (item, index) =>
            if (index % 3 == i - 1) system.b(index / 3) else item.right.get
          }
      }


      def det(i: Int) = {
        val m = matrix(i)

        def valAt(i: Int, j: Int) = m(i * system.A.width + j)

        val det0 = valAt(1, 1) * valAt(2, 2) - valAt(1, 2) * valAt(2, 1)
        val det1 = valAt(0, 1) * valAt(2, 2) - valAt(0, 2) * valAt(2, 1)
        val det2 = valAt(0, 1) * valAt(1, 2) - valAt(0, 2) * valAt(1, 1)
        valAt(0, 0) * det0 + valAt(1, 0) * det1 + valAt(2, 0) * det2
      }

      //      require(system.A.width == 3)
      //      require(system.A.height == 3)

      val result = system.x.view.zipWithIndex.map { case (variable, index) =>
        variable.copy(value = Some(det(index)))
      }.force

      (result, SolutionCount.One)
    }
  }
}

