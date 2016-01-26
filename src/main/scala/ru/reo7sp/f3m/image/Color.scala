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

import ru.reo7sp.f3m.image.Color.{ColorValuesParser, RGB}

import scala.math._

case class Color(alpha: Double, red: Double, green: Double, blue: Double) extends Ordered[Color] {
  require(alpha >= 0 && alpha <= 1)
  require(red >= 0 && red <= 1)
  require(green >= 0 && green <= 1)
  require(blue >= 0 && blue <= 1)

  def argb = alphaInt << 24 | redInt << 16 | greenInt << 8 | blueInt

  def alphaInt = (alpha * 255).toInt
  def redInt   = (red * 255).toInt
  def greenInt = (green * 255).toInt
  def blueInt  = (blue * 255).toInt

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

  def differenceSqr(other: Color) = pow(other.lightness - lightness, 2) + pow(other.a - a, 2) + pow(other.b - b, 2)
  def difference(other: Color) = sqrt(differenceSqr(other))

  def copy(a: Double = alpha, v1: Double = red, v2: Double = green, v3: Double = blue)(implicit parser: ColorValuesParser = RGB): Color = parser(a, v1, v2, v3)

  override def compare(other: Color): Int = differenceSqr(other) match {
    case 0 => 0
    case x if x > 0 => 1
    case _ => -1
  }

  private def minComponent = min(min(red, green), blue)
  private def maxComponent = max(max(red, green), blue)
}

object Color {
  def apply(argb: Int): Color = new Color((argb & 0xff000000) >> 24, (argb & 0x00ff0000) >> 16, (argb & 0x0000ff00) >> 8, argb & 0x000000ff)
  def apply(a: Double, v1: Double, v2: Double, v3: Double)(implicit parser: ColorValuesParser = RGB): Color = parser(a, v1, v2, v3)

  implicit class IntWrapper(val i: Int) extends AnyVal {
    def toColor = Color(i)
  }

  trait ColorValuesParser {
    def apply(a: Double, v1: Double, v2: Double, v3: Double): Color
  }

  object RGB extends ColorValuesParser {
    override def apply(a: Double, r: Double, g: Double, b: Double): Color = new Color(a, r, g, b)
  }

  object HSB extends ColorValuesParser {
    override def apply(a: Double, h: Double, s: Double, b: Double): Color = {
      if (s == 0) {
        new Color(a, b, b, b)
      } else {
        val region = (h * 6).toInt
        val remainder = h * 6 - region

        val p = b * (1 - s)
        val q = b * (1 - s * remainder)
        val t = b * (1 - s * (1 - remainder))

        region match {
          case 0 => new Color(a, b, t, p)
          case 1 => new Color(a, q, b, p)
          case 2 => new Color(a, p, b, t)
          case 3 => new Color(a, p, q, b)
          case 4 => new Color(a, t, p, b)
          case _ => new Color(a, b, p, q)
        }
      }
    }
  }

  val HSV = HSB

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
        new Color(a, l, l, l)
      } else {
        val q = if (l < 0.5) l * (1 + s) else l + s - l * s
        val p = 2 * l - q
        new Color(a, hue2rgb(p, q, h + 1.0 / 3), hue2rgb(p, q, h), hue2rgb(p, q, h - 1.0 / 3))
      }
    }
  }

  object LAB extends ColorValuesParser {
    override def apply(alpha: Double, l: Double, a: Double, b: Double): Color = {
      val s = sqrt(a * a + b * b)
      HSL(alpha, asin(a * s), s, l)
    }
  }
}
