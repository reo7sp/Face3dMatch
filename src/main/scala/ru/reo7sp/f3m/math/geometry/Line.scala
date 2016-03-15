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

package ru.reo7sp.f3m.math.geometry

import ru.reo7sp.f3m.math.geometry.GeometricVector._
import ru.reo7sp.f3m.math.geometry.Point.SeqOfDoubleToPointWrapper
import ru.reo7sp.f3m.math.linear.{LinearEquationsSystem, Matrix, Var}

case class Line(initialPoint: Point, params: Double*) {
  def dimension = params.size

  def has(point: Point) = {
    val values = (point.coords, initialPoint.coords, params).zipped.toIterable
    val firstResult = values.head match {
      case (x, x0, a) => (x - x0) / a
    }
    values.tail.forall {
      case (x, x0, a) => (x - x0) / a == firstResult
    }
  }

  def findIntersection(other: Line) = {
    val (answer, ansCount) = LinearEquationsSystem(
      A = Matrix(Size(dimension, 2), (params.view ++ other.params).map(Right(_))(collection.breakOut)),
      x = Seq(Var[Double]('x), Var[Double]('y), Var[Double]('z)),
      b = Seq(0.0, 0.0)
    ).solve

    if (ansCount == LinearEquationsSystem.SolutionCount.Zero) None else Option(answer.map(_.get).toPoint)
  }

  def commonPerpendicularWith(other: Line) = (params.toGeometricVector cross other.params.toGeometricVector).normalize

  def findMinDistance(other: Line) = {
    val p = this commonPerpendicularWith other
    val d = initialPoint.toGeometricVector - other.initialPoint.toGeometricVector
    (p dot d).abs
  }

  def findMinDistancePoint(other: Line) = {
    val A = params.view.map(-_) ++ other.params ++ commonPerpendicularWith(other).coords.view.map(-_)
    val (answer, _) = LinearEquationsSystem(
      A = Matrix(Size(dimension, 3), A.map(Right(_))(collection.breakOut)).transpose,
      x = Seq(Var[Double]('t1), Var[Double]('t2), Var[Double]('l)),
      b = (initialPoint.coords zip other.initialPoint.coords).map { case (x1, x2) => x1 - x2 }
    ).solve(LinearEquationsSystem.KramerSolver)

    initialPoint + params.view.map(_ * answer.head.get).toPoint
  }
}

object Line {
  def apply(first: Point, second: Point): Line = {
    val vectorCoords = (first zip second).map { case (x1, x2) => x2 - x1 }
    Line(first, vectorCoords: _*)
  }
}
