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

package ru.reo7sp.f3m.image.edit

import ru.reo7sp.f3m.image.Image._
import ru.reo7sp.f3m.image.{Image, ImageCompanion, Pixel}

import scala.math._

package object transform {
  def rotate[T <: Image](image: T, angle: Double)(implicit imageCompanion: ImageCompanion[T]) = {
    image.pixels.map { case Pixel(point, color) =>
      val newPoint = point.copy(
        x = cos(angle) * point.x - sin(angle) * point.y,
        y = sin(angle) * point.x + cos(angle) * point.y
      )
      Pixel(newPoint, color)
    }.filter { case Pixel(point, _) =>
      point.x >= 0 && point.y >= 0
    }.toImage[T]
  }
}
