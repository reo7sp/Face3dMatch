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

import ru.reo7sp.f3m.math.NumExtensions.DoubleWrapper
import ru.reo7sp.f3m.math.geometry.Point.SeqOfDouble

import scala.collection.SeqLike
import scala.collection.generic.GenericTraversableTemplate
import scala.math.sqrt

case class Point(coords: Double*) extends Seq[Double] with GenericTraversableTemplate[Double, Point] with SeqLike[Double, Point] {
  def apply(i: Int) = if (dimension > i) coords(i) else 0.0
  def dimension = coords.size

  def x = apply(0)
  def y = apply(1)
  def z = apply(2)
  def w = apply(3)

  def +(other: Point) = zipAll(other, 0.0, 0.0).map(t => t._1 + t._2).toPoint
  def -(other: Point) = zipAll(other, 0.0, 0.0).map(t => t._1 - t._2).toPoint

  def distanceSqr(other: Point) = coords.zipAll(other.coords, 0.0, 0.0).foldLeft(0.0)((r, t) => r + (t._1 - t._2).squared)
  def distance(other: Point) = sqrt(distanceSqr(other))

  def copy(x: Double = x, y: Double = y, z: Double = z, w: Double = w) = {
    val r = coords.toBuffer
    for (i <- 0 until dimension) {
      i match {
        case 0 => r(i) = x
        case 1 => r(i) = y
        case 2 => r(i) = z
        case 3 => r(i) = w
      }
    }
    Point(r: _*)
  }

  override def iterator: Iterator[Double] = coords.iterator
  override def length: Int = coords.length
}

object Point {
  def zero(dimension: Int) = Point(Seq.fill(dimension)(0): _*)

  implicit class SeqOfDouble(s: Seq[Double]) {
    def toPoint = Point(s: _*)
  }

}