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

import ru.reo7sp.f3m.math.geometry.{Point, Size}

trait ImageCompanion[Repr <: Image] {
  def ofSize(size: Size): Repr

  def fromPixels(pixels: TraversableOnce[Pixel]): Repr = {
    val (iter1, iter2) = pixels.toIterator.duplicate
    val (w, h) = iter1.foldLeft((0.0, 0.0)) { case ((maxX, maxY), Pixel(Point(x, y), _)) => (maxX max x, maxY max y) }
    fromPixels(iter2, Size(w.toInt, h.toInt))
  }

  def fromPixels(iter: TraversableOnce[Pixel], size: Size): Repr

  implicit def imageCompanion: ImageCompanion[Repr] = this
}
