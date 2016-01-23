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

package ru.reo7sp.f3m.image

import scala.math._

case class Point3D(x: Int, y: Int, z: Int) {
  def +(other: Point3D) = new Point3D(x + other.x, y + other.y, z + other.z)
  def -(other: Point3D) = new Point3D(x - other.x, y - other.y, z - other.z)

  def distanceSqr(other: Point3D) = pow(x - other.x, 2) + pow(y - other.y, 2)  + pow(z - other.z, 2)

  def distance(other: Point3D): Double = {
    if (other.x == x) {
      abs(y - other.y)
    } else if (other.y == y) {
      abs(x - other.x)
    } else if (other.z == z) {
      abs(z - other.z)
    } else {
      sqrt(distanceSqr(other))
    }
  }

  def to2D = Point(x, y)
}

