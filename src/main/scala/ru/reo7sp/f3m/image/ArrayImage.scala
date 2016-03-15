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

class ArrayImage(val size: Size, val handle: mutable.Map[Point, Color] = new mutable.HashMap[Point, Color]) extends Image with ImageMutability with ImageCopyability[ArrayImage] {

  override def apply(p: Point): Color = handle.getOrElse(p, Color.BLACK)
  override def update(p: Point, c: Color): Unit = handle(p) = c

  override def copy(rect: Rect, size: Size): ArrayImage = {
    val (scaleX, scaleY) = rect.toSize / size
    val img = ArrayImage ofSize size
    size.points.foreach { point =>
      img(point) = this (Point((rect.x + point.x * scaleX).floor, (rect.y + point.y * scaleY).floor))
    }
    img
  }

  override def toString = s"ArrayImage($size)"
}

object ArrayImage extends ImageCompanion[ArrayImage] {
  override def ofSize(size: Size): ArrayImage = new ArrayImage(size)

  override def fromPixels(pixels: TraversableOnce[Pixel], size: Size): ArrayImage = {
    val pixelsAsTuples = pixels.toStream.view.map(pixel => (pixel.point, pixel.color))
    new ArrayImage(size, mutable.Map(pixelsAsTuples: _*))
  }

  implicit class ImageToArrayImageWrapper(img: Image) {
    def toArrayImage = ArrayImage.fromPixels(img.pixels, img.size)
  }
}
