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

import ru.reo7sp.f3m.math.NumExtensions._
import ru.reo7sp.f3m.math.geometry.Point.SeqOfDouble
import ru.reo7sp.f3m.math.linear.{LinearEquationsSystem, Matrix, Var}

case class Line(initialPoint: Point, params: Double*) {
  def dimension = params.size - 1

  def apply(point: Point) = {
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
      A = Matrix(Size(dimension, 2), (params ++ other.params).map(Right(_))),
      x = Seq(Var[Double]('x), Var[Double]('y), Var[Double]('z)),
      b = Seq(0.0, 0.0)
    ).solve

    if (ansCount == LinearEquationsSystem.SolutionCount.Zero) None else Option(answer.map(_.value.get).toPoint)
  }

  def commonPerpendicularWith(other: Line) = {
    val vector1 = params
    val vector2 = other.params
    val perpendicular = Seq(
      vector1(1) * vector2(2) - vector1(2) * vector1(1),
      vector1(0) * vector2(2) - vector1(2) * vector1(0),
      vector1(0) * vector2(1) - vector1(1) * vector1(0)
    )
    val perpendicularLength = math.sqrt(perpendicular.map(_.squared).sum)
    perpendicular.map(_ / perpendicularLength)
  }

  def findMinDistance(other: Line) = {
    val perpendicular = this commonPerpendicularWith other
    val deltaVector = (initialPoint.coords zip other.initialPoint.coords).map { case (x1, x2) => x1 - x2 }
    (perpendicular zip deltaVector).map { case (x1, x2) => x1 * x2 }.sum.abs
  }

  def findMinDistancePoint(other: Line): Point = {
    val distance = this findMinDistance other
    ???
  }
}

object Line {
  def apply(first: Point, second: Point): Line = {
    val vectorCoords = (first.coords zip second.coords).map { case (x1, x2) => x2 - x1 }
    Line(first, vectorCoords: _*)
  }
}
