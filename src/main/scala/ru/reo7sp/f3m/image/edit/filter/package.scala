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

import ru.reo7sp.f3m.image.Color.HSB
import ru.reo7sp.f3m.image.Image.TraversableOfPixelWrapper
import ru.reo7sp.f3m.image.{Color, Image, ImageCompanion, Pixel}
import ru.reo7sp.f3m.math.NumExtensions._

package object filter {
  def desaturated[T <: Image](image: T)(implicit imageCompanion: ImageCompanion[T]) = image.pixels.map { case Pixel(point, color) =>
    Pixel(point, color.copy(v2 = 0)(HSB))
  }.toImage[T](image.size)

  def contrasted[T <: Image](image: T, value: Double)(implicit imageCompanion: ImageCompanion[T]) = image.pixels.map { case Pixel(point, color) =>
    val factor = value.squared
    val r = factor * (color.red - 0.5) + 0.5
    val g = factor * (color.green - 0.5) + 0.5
    val b = factor * (color.blue - 0.5) + 0.5
    Pixel(point, Color(color.alpha, r, g, b))
  }.toImage[T](image.size)
}
