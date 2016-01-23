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

import scala.language.implicitConversions

class AndroidImage(val handle: Bitmap) extends Image {
  override def width: Int = handle.getWidth
  override def height: Int = handle.getWidth

  override def apply(p: Point): Color = handle.getPixel(p.x, p.y).toColor

  override def copy(rect: Rect): AndroidImage = new AndroidImage(handle) {
    override def width: Int = rect.width
    override def height: Int = rect.height

    override def apply(p: Point): Color = super.apply(p + rect.topLeft)
  }
}

object AndroidImage {
  implicit def bitmapToImage(a: Bitmap): AndroidImage = new AndroidImage(a)
  implicit def imageToBitmap(a: AndroidImage): Bitmap = a.handle
}