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

package ru.reo7sp.f3m.image.understand

import ru.reo7sp.f3m.image.{Image, Pixel}
import ru.reo7sp.f3m.math.geometry.Point

package object content {
  def findEdges(image: Image, edgeThreshold: Double = 0.5): Iterator[Point] = {
    val thresholdSqr = edgeThreshold * edgeThreshold

    val a = (image.pixels zip image.pixels.drop(1)).filter { case (Pixel(_, color1), Pixel(_, color2)) =>
      color1.differenceSqr(color2) > thresholdSqr
    }.map { case (Pixel(point, _), _) =>
      point
    }

    val b = (image.rows zip image.rows.drop(1)).flatMap { case (row1, row2) =>
      (row1 zip row2).filter { case (Pixel(_, color1), Pixel(_, color2)) =>
        color1.differenceSqr(color2) > thresholdSqr
      }.map { case (Pixel(point, _), _) =>
        point
      }
    }

    a ++ b
  }
}
