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

import android.graphics.Bitmap
import ru.reo7sp.f3m.image.Color._
import ru.reo7sp.f3m.math.geometry.{Point, Rect, Size}

import scala.language.implicitConversions

class AndroidImage(val handle: Bitmap) extends Image with ImageMutability with ImageCopyability[AndroidImage] {
  require(handle != null)

  override val size = Size(handle.getWidth, handle.getWidth)

  override def apply(p: Point) = handle.getPixel(p.x.toInt, p.y.toInt).toColor
  override def update(p: Point, c: Color) = handle.setPixel(p.x.toInt, p.y.toInt, c.argb)

  override def copy(rect: Rect, scale: Double): AndroidImage = {
    var h = handle
    if (rect != size.toRect) {
      h = Bitmap.createBitmap(h, rect.x.toInt, rect.y.toInt, rect.width.toInt, rect.height.toInt)
    }
    if (scale != 1) {
      h = Bitmap.createScaledBitmap(h, (rect.width * scale).toInt, (rect.height * scale).toInt, true)
    }
    new AndroidImage(h)
  }
}

object AndroidImage extends ImageCompanion[AndroidImage] {
  implicit def bitmapToImage(a: Bitmap): AndroidImage = new AndroidImage(a)
  implicit def imageToBitmap(a: AndroidImage): Bitmap = a.handle

  override def apply(size: Size) = new AndroidImage(Bitmap.createBitmap(size.width, size.height, Bitmap.Config.ARGB_8888))

  override def apply(pixels: TraversableOnce[Pixel], size: Size) = {
    val img = AndroidImage(size)
    pixels.foreach(img.update)
    img
  }
}
