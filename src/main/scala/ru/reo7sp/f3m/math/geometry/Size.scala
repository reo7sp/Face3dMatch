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

case class Size(width: Int, height: Int) {
  require(width >= 0 && height >= 0)

  def +(other: Size): Size = Size(width + other.width, height + other.height)
  def -(other: Size): Size = Size(width - other.width, height - other.height)
  def *(scaleX: Double, scaleY: Double): Size = Size((width * scaleX).toInt, (height * scaleY).toInt)
  def /(scaleX: Double, scaleY: Double): Size = this *(1 / scaleX, 1 / scaleY)
  def *(scale: Double): Size = this *(scale, scale)
  def /(scale: Double): Size = this /(scale, scale)

  def /(other: Size): (Double, Double) = (width.toDouble / other.width, height.toDouble / other.height)

  def area = width * height
  def aspectRatio = width / height
  def invAspectRatio = 1 / aspectRatio

  def toRect = Rect(Point.zero(2), Point(width, height))
}
