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

import ru.reo7sp.f3m.math.geometry.{Point, Rect, Size}

import scala.collection.mutable

class ArrayImage(val size: Size) extends Image with ImageMutability with ImageCopyability[ArrayImage] {
  val handle = new mutable.HashMap[Point, Color]

  override def apply(p: Point): Color = handle.getOrElse(p, Color.TRANSPARENT)
  override def update(p: Point, c: Color): Unit = handle(p) = c

  override def copy(rect: Rect, scale: Double): ArrayImage = {
    val pixels = handle.iterator.filter { case (point, _) =>
      rect.has(point)
    }.map { case (point, color) =>
      Pixel(point.copy(point.x * scale, point.y * scale), color)
    }
    ArrayImage fromPixels pixels
  }
}

object ArrayImage extends ImageCompanion[ArrayImage] {
  override def ofSize(size: Size): ArrayImage = new ArrayImage(size)

  override def fromPixels(pixels: TraversableOnce[Pixel], size: Size): ArrayImage = {
    val img = ArrayImage ofSize size
    pixels.foreach(img.update)
    img
  }

  implicit class AndroidImageWrapper(img: AndroidImage) {
    def toArrayImage = ArrayImage.fromPixels(img.pixels, img.size)
  }
}
