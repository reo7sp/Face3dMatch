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

import ru.reo7sp.f3m.math.linear.{LinearEquationsSystem, Matrix, Var}
import Point.SeqOfDouble

case class Line(initialPoint: Point, params: Double*) {
  def dimension = params.size

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
    val result = LinearEquationsSystem(
      Matrix(Size(dimension, 2), params ++ other.params),
      Seq(Var[Double]('x), Var[Double]('y), Var[Double]('z)),
      Seq(0.0, 0.0)
    ).solve

    if (result._2 == LinearEquationsSystem.SolutionCount.Zero) {
      None
    } else {
      Some(result._1.map(_.value.get).toPoint)
    }
  }
}

object Line {
  def apply(first: Point, second: Point) = Line(first, first.coords.zip(second.coords).map(t => t._2 - t._1): _*)
}
