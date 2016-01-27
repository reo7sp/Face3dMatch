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
import ru.reo7sp.f3m.image.Color.IntWrapper
import ru.reo7sp.f3m.math.geometry.{Point, Rect}

import scala.language.implicitConversions

class AndroidImage(val handle: Bitmap) extends Image {
  override val width = handle.getWidth
  override val height = handle.getWidth

  override def apply(p: Point): Color = handle.getPixel(p.x.toInt, p.y.toInt).toColor
  override def update(p: Point, c: Color): Unit = handle.setPixel(p.x.toInt, p.y.toInt, c.argb)

  override def copy(rect: Rect, scale: Double) = {
    var h = handle
    if (rect.x != 0 || rect.y != 0 || rect.width != width || rect.height != height) {
      h = Bitmap.createBitmap(h, rect.x.toInt, rect.y.toInt, rect.width.toInt, rect.height.toInt)
    }
    if (scale != 1.0) {
      h = Bitmap.createScaledBitmap(h, (width * scale).toInt, (height * scale).toInt, true)
    }
    new AndroidImage(h)
  }
}

object AndroidImage {
  implicit def bitmapToImage(a: Bitmap): AndroidImage = new AndroidImage(a)
  implicit def imageToBitmap(a: AndroidImage): Bitmap = a.handle
}