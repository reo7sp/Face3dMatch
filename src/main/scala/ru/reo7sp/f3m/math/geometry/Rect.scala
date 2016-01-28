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

case class Rect(topLeft: Point, bottomRight: Point) {
  require(topLeft.x < bottomRight.x && topLeft.y > bottomRight.y)

  def x = topLeft.x
  def y = topLeft.y
  def width = (topLeft.x - bottomRight.x).abs
  def height = (topLeft.y - bottomRight.y).abs
  def area = width * height
}

object Rect {
  def apply(p1: Point, p2: Point) = new Rect(Point(p1.x min p2.x, p1.y max p2.y), Point(p1.x max p2.x, p1.y min p2.y))

  implicit class PointWrapper(val p: Point) {
    def toRect(other: Point) = Rect(p, other)
  }

}