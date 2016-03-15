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

import ru.reo7sp.f3m.image.Color.{ARGB8888, ColorIntegerValueParser, ColorValuesParser, RGB}
import ru.reo7sp.f3m.math.NumExtensions.DoubleExtensions

import scala.math._

class Color(val alpha: Double, val red: Double, val green: Double, val blue: Double) extends Ordered[Color] with Product4[Double, Double, Double, Double] {
  def alphaInt = (alpha * 255).toInt
  def redInt = (red * 255).toInt
  def greenInt = (green * 255).toInt
  def blueInt = (blue * 255).toInt

  def toInt(implicit parser: ColorIntegerValueParser = ARGB8888) = parser(this)
  def argb = toInt(ARGB8888)

  def hue = {
    val min = minComponent
    val max = maxComponent

    if (min == max) {
      0.0
    } else {
      val d = max - min
      val result = (max match {
        case `red` => (green - blue) / d
        case `green` => (blue - red) / d + 2
        case `blue` => (red - green) / d + 4
      }) / 6.0
      result + (if (result < 0) 1 else 0)
    }
  }

  def saturation = {
    val min = minComponent
    val max = maxComponent

    if (min == max) {
      0.0
    } else if (lightness > 0.5) {
      (max - min) / (2 - max - min)
    } else {
      (max - min) / (max + min)
    }
  }

  def brightness = maxComponent
  def lightness = (minComponent + maxComponent) / 2

  def a = sin(hue * 2 * Pi) * saturation
  def b = cos(hue * 2 * Pi) * saturation

  def differenceSqr(other: Color) = (other.lightness - lightness).squared + (other.a - a).squared + (other.b - b).squared
  def difference(other: Color) = sqrt(differenceSqr(other))

  def copy(a: Double = -1, v1: Double = -1, v2: Double = -1, v3: Double = -1)(implicit parser: ColorValuesParser = RGB): Color = {
    parser(
      if (a == -1) parser(this, 0) else a,
      if (v1 == -1) parser(this, 1) else v1,
      if (v2 == -1) parser(this, 2) else v2,
      if (v3 == -1) parser(this, 3) else v3
    )
  }

  def minComponent = red min green min blue
  def maxComponent = red max green max blue

  override def compare(other: Color) = differenceSqr(other) match {
    case 0 => 0
    case x if x > 0 => 1
    case _ => -1
  }

  override def _1 = alpha
  override def _2 = red
  override def _3 = green
  override def _4 = blue
  override def productPrefix = "Color"

  override def canEqual(that: Any) = that match {
    case other: Product4[Double, Double, Double, Double] => _1 == other._1 && _2 == other._2 && _3 == other._3 && _4 == other._4
    case _ => false
  }

  override def toString = s"Color($alpha, $red, $green, $blue)"
}

object Color {
  val BLACK = new Color(1, 0, 0, 0)
  val DARK_GRAY = new Color(1, 0.25, 0.25, 0.25)
  val GRAY = new Color(1, 0.5, 0.5, 0.5)
  val LIGHT_GRAY = new Color(1, 0.75, 0.75, 0.75)
  val WHITE = new Color(1, 1, 1, 1)
  val MAGENTA = new Color(1, 1, 0, 1)
  val RED = new Color(1, 1, 0, 0)
  val YELLOW = new Color(1, 1, 1, 0)
  val GREEN = new Color(1, 0, 1, 0)
  val CYAN = new Color(1, 0, 1, 1)
  val BLUE = new Color(1, 0, 0, 1)
  val TRANSPARENT = new Color(0, 0, 0, 0)

  def fromComponents(a: Double, v1: Double, v2: Double, v3: Double)(implicit parser: ColorValuesParser = RGB) = parser(a, v1, v2, v3)
  def fromInteger(value: Int)(implicit parser: ColorIntegerValueParser = ARGB8888) = parser(value)

  def apply(a: Double, v1: Double, v2: Double, v3: Double)(implicit parser: ColorValuesParser = RGB): Color = fromComponents(a, v1, v2, v3)(parser)
  def unapply(color: Color): Option[(Double, Double, Double, Double)] = Some((color.alpha, color.red, color.green, color.blue))

  implicit class IntToColorWrapper(val i: Int) extends AnyVal {
    def toColor(implicit parser: ColorIntegerValueParser = ARGB8888) = Color.fromInteger(i)(parser)
  }

  trait ColorValuesParser {
    def apply(a: Double, v1: Double, v2: Double, v3: Double): Color
    def apply(from: Color, valueIndex: Int): Double
  }

  object RGB extends ColorValuesParser {
    override def apply(a: Double, r: Double, g: Double, b: Double): Color = {
      new Color(1 /*a.clamp(0, 1)*/ , r.clamp(0, 1), g.clamp(0, 1), b.clamp(0, 1)) // HACK
    }
    override def apply(color: Color, valueIndex: Int): Double = color.productElement(valueIndex).asInstanceOf[Double]
  }

  object HSB extends ColorValuesParser {
    override def apply(a: Double, h: Double, s: Double, b: Double): Color = {
      if (s == 0) {
        RGB(a, b, b, b)
      } else {
        val region = (h * 6).toInt
        val remainder = h * 6 - region

        val p = b * (1 - s)
        val q = b * (1 - s * remainder)
        val t = b * (1 - s * (1 - remainder))

        region match {
          case 0 => RGB(a, b, t, p)
          case 1 => RGB(a, q, b, p)
          case 2 => RGB(a, p, b, t)
          case 3 => RGB(a, p, q, b)
          case 4 => RGB(a, t, p, b)
          case _ => RGB(a, b, p, q)
        }
      }
    }

    override def apply(color: Color, valueIndex: Int): Double = valueIndex match {
      case 0 => color.alpha
      case 1 => color.hue
      case 2 => color.saturation
      case 3 => color.brightness
    }
  }

  def HSV = HSB

  object HSL extends ColorValuesParser {
    override def apply(a: Double, h: Double, s: Double, l: Double): Color = {
      def hue2rgb(p: Double, q: Double, tRaw: Double) = {
        val t = if (tRaw < 0) {
          tRaw + 1
        } else if (tRaw > 1) {
          tRaw - 1
        } else {
          tRaw
        }
        if (t < 1.0 / 6) {
          p + (q - p) * 6 * t
        } else if (t < 1.0 / 2) {
          q
        } else if (t < 2.0 / 3) {
          p + (q - p) * (2.0 / 3 - t) * 6
        } else {
          p
        }
      }

      if (s == 0) {
        RGB(a, l, l, l)
      } else {
        val q = if (l < 0.5) l * (1 + s) else l + s - l * s
        val p = 2 * l - q
        RGB(a, hue2rgb(p, q, h + 1.0 / 3), hue2rgb(p, q, h), hue2rgb(p, q, h - 1.0 / 3))
      }
    }

    override def apply(color: Color, valueIndex: Int): Double = valueIndex match {
      case 0 => color.alpha
      case 1 => color.hue
      case 2 => color.saturation
      case 3 => color.lightness
    }
  }

  object LAB extends ColorValuesParser {
    override def apply(alpha: Double, l: Double, a: Double, b: Double): Color = {
      val s = sqrt(a * a + b * b)
      HSL(alpha, asin(a * s), s, l)
    }

    override def apply(color: Color, valueIndex: Int): Double = valueIndex match {
      case 0 => color.alpha
      case 1 => color.lightness
      case 2 => color.a
      case 3 => color.b
    }
  }

  trait ColorIntegerValueParser {
    def apply(value: Int): Color
    def apply(color: Color): Int
  }

  object ARGB8888 extends ColorIntegerValueParser {
    override def apply(value: Int): Color = {
      val alphaInt = (value & 0xff000000) >> 24
      val redInt = (value & 0x00ff0000) >> 16
      val greenInt = (value & 0x0000ff00) >> 8
      val blueInt = value & 0x000000ff
      RGB(alphaInt / 255.0, redInt / 255.0, greenInt / 255.0, blueInt / 255.0)
    }

    override def apply(color: Color): Int = color.alphaInt << 24 | color.redInt << 16 | color.greenInt << 8 | color.blueInt
  }

  object ARGB4444 extends ColorIntegerValueParser {
    override def apply(value: Int): Color = {
      val alphaInt = (value & 0xf000) >> 12
      val redInt = (value & 0x0f00) >> 8
      val greenInt = (value & 0x00f0) >> 4
      val blueInt = value & 0x000f
      RGB(alphaInt / 127.0, redInt / 127.0, greenInt / 127.0, blueInt / 127.0)
    }

    override def apply(color: Color): Int = color.alphaInt >> 1 << 12 | color.redInt >> 1 << 8 | color.greenInt >> 1 << 4 | color.blueInt >> 1
  }

  object ALPHA8 extends ColorIntegerValueParser {
    override def apply(value: Int): Color = RGB(value / 255.0, 0, 0, 0)
    override def apply(color: Color): Int = color.alphaInt
  }

  object RGB565 extends ColorIntegerValueParser {
    override def apply(value: Int): Color = {
      val redInt = (value & 0xf800) >> 11
      val greenInt = (value & 0x7e0) >> 6
      val blueInt = value & 0x1f
      RGB(1, redInt / 31.0, greenInt / 63.0, blueInt / 31.0)
    }

    override def apply(color: Color): Int = (color.red * 31).toInt << 11 | (color.green * 63).toInt << 6 | (color.blue * 31).toInt
  }
}
