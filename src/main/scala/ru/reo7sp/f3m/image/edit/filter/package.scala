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

import ru.reo7sp.f3m.image.Color.{HSB, RGB}
import ru.reo7sp.f3m.image.Image.TraversableOfPixelWrapper
import ru.reo7sp.f3m.image.{Image, Pixel}

package object filter {
  def desaturated(image: Image): Image = image.pixels.map {
    case Pixel(point, color) => Pixel(point, color.copy(v2 = 0)(HSB))
  }.toImage

  def contrasted(image: Image, by: Double): Image = image.pixels.map {
    case Pixel(point, color) => {
      val factor = (1.016 * (by + 1)) / (1.016 - by)
      val r = (factor * (color.red   - 0.5) + 0.5) min 1 max 0
      val g = (factor * (color.green - 0.5) + 0.5) min 1 max 0
      val b = (factor * (color.blue  - 0.5) + 0.5) min 1 max 0
      Pixel(point, color.copy(v1 = r, v2 = g, v3 = b)(RGB))
    }
  }.toImage
}
