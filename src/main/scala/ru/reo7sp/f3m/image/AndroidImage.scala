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
import ru.reo7sp.f3m.image.AndroidImage._
import ru.reo7sp.f3m.image.Color._
import ru.reo7sp.f3m.math.geometry.{Point, Rect, Size}

import scala.language.implicitConversions

class AndroidImage(val handle: Bitmap) extends Image with ImageMutability with ImageCopyability[AndroidImage] {
  require(handle != null)

  override val size = Size(handle.getWidth, handle.getWidth)

  override def apply(p: Point) = handle.getPixel(p.x.toInt, p.y.toInt).toColor(handle.getConfig)
  override def update(p: Point, c: Color) = handle.setPixel(p.x.toInt, p.y.toInt, c.toInt(handle.getConfig))

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

  implicit def bitmapConfigToColorIntegerValueParser(a: Bitmap.Config): ColorIntegerValueParser = a match {
    case Bitmap.Config.ALPHA_8 => ALPHA8
    case Bitmap.Config.ARGB_4444 => ARGB4444
    case Bitmap.Config.ARGB_8888 => ARGB8888
    case Bitmap.Config.RGB_565 => RGB565
  }

  implicit def colorIntegerValueParserToBitmapConfig(a: ColorIntegerValueParser): Bitmap.Config = a match {
    case ALPHA8 => Bitmap.Config.ALPHA_8
    case ARGB4444 => Bitmap.Config.ARGB_4444
    case ARGB8888 => Bitmap.Config.ARGB_8888
    case RGB565 => Bitmap.Config.RGB_565
  }

  def apply(size: Size, config: Bitmap.Config) = new AndroidImage(Bitmap.createBitmap(size.width, size.height, config))

  def apply(pixels: TraversableOnce[Pixel], size: Size, config: Bitmap.Config) = {
    val img = AndroidImage(size, config)
    pixels.foreach(img.update)
    img
  }

  override def apply(size: Size) = AndroidImage(size, Bitmap.Config.ARGB_8888)
  override def apply(pixels: TraversableOnce[Pixel], size: Size) = AndroidImage(pixels, size, Bitmap.Config.ARGB_8888)
}
