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

import ru.reo7sp.f3m.math.NumExtensions.DoubleExtensions
import ru.reo7sp.f3m.math.geometry.Point.SeqOfDoubleToPointWrapper

import scala.math.sqrt

case class Point(coords: Double*) {
  def apply(i: Int) = if (dimension > i) coords(i) else 0.0

  def x = apply(0)
  def y = apply(1)
  def z = apply(2)

  def map(f: Double => Double) = Point(coords.map(f): _*)
  def zip(other: Point) = coords.zipAll(other.coords, 0.0, 0.0)

  def +(other: Point) = (this zip other).map { case (x1, x2) => x1 + x2 }.toPoint
  def -(other: Point) = (this zip other).map { case (x1, x2) => x1 - x2 }.toPoint
  def *(d: Double) = map(_ * d)
  def /(d: Double) = this * (1 / d)

  def distanceSqr(other: Point) = (this zip other).map { case (x1, x2) => (x1 - x2).squared }.sum
  def distance(other: Point) = sqrt(distanceSqr(other))

  def copy(x: Double = x, y: Double = y, z: Double = z) = {
    val newCoords = coords.toBuffer
    newCoords.indices.foreach {
      case 0 => newCoords(0) = x
      case 1 => newCoords(1) = y
      case 2 => newCoords(2) = z
    }
    Point(newCoords: _*)
  }

  def dimension = coords.size
}

object Point {
  def zero(dimension: Int) = Point(Seq.fill(dimension)(0.0): _*)

  implicit class SeqOfDoubleToPointWrapper(s: Seq[Double]) {
    def toPoint = Point(s: _*)
  }

}