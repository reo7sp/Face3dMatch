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
import ru.reo7sp.f3m.math.geometry.GeometricVector._

import scala.math._

case class GeometricVector(coords: Double*) {
  def apply(i: Int) = if (dimension > i) coords(i) else 0.0
  def x = apply(0)
  def y = apply(1)
  def z = apply(2)

  def angle(i: Int) = acos(apply(i) / length)
  def angleX = angle(0)
  def angleY = angle(1)
  def angleZ = angle(2)

  def map(f: Double => Double) = GeometricVector(coords.map(f): _*)
  def zip(other: GeometricVector) = coords.zipAll(other.coords, 0.0, 0.0)

  def +(other: GeometricVector) = (this zip other).map { case (x1, x2) => x1 + x2 }.toGeometricVector
  def -(other: GeometricVector) = (this zip other).map { case (x1, x2) => x1 - x2 }.toGeometricVector
  def *(d: Double) = map(_ * d)
  def /(d: Double) = this * (1 / d)

  def cross(other: GeometricVector) = {
    val a = coords
    val b = other.coords
    GeometricVector(
      a(1) * b(2) - a(2) * a(1),
      a(0) * b(2) - a(2) * a(0),
      a(0) * b(1) - a(1) * a(0)
    )
  }

  def dot(other: GeometricVector) = (this zip other).map { case (x1, x2) => x1 * x2 }.sum

  def lengthSqr = coords.map(_.squared).sum
  def length = sqrt(lengthSqr)

  def normalized = this / length

  def copy(x: Double = x, y: Double = y, z: Double = z) = {
    val newCoords = coords.toBuffer
    newCoords.indices.foreach {
      case 0 => newCoords(0) = x
      case 1 => newCoords(1) = y
      case 2 => newCoords(2) = z
    }
    GeometricVector(newCoords: _*)
  }

  def dimension = coords.size

  def toPoint = Point(coords: _*)
}

object GeometricVector {
  def zero(dimension: Int) = GeometricVector(Seq.fill(dimension)(0.0): _*)

  def apply(first: Point, second: Point): GeometricVector = {
    val vectorCoords = (first.coords zip second.coords).map { case (x1, x2) => x2 - x1 }
    GeometricVector(vectorCoords: _*)
  }

  implicit class SeqOfDoubleToVectorWrapper(s: Seq[Double]) {
    def toGeometricVector = GeometricVector(s: _*)
  }

  implicit class PointToVectorWrapper(p: Point) {
    def toGeometricVector = GeometricVector(p.coords: _*)
  }

}
