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

import scala.math._

case class Color(argb: Int) extends Ordered[Color] {
  def alphaInt = (argb & 0xff000000) >> 24
  def redInt   = (argb & 0x00ff0000) >> 16
  def greenInt = (argb & 0x0000ff00) >> 8
  def blueInt  =  argb & 0x000000ff

  val alpha = alphaInt / 255.0
  val red   = redInt / 255.0
  val green = greenInt / 255.0
  val blue  = blueInt / 255.0

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

  def a = sin(hue * 2 * Pi / saturation)
  def b = cos(hue * 2 * Pi / saturation)

  def differenceSqr(other: Color) = pow(other.lightness - lightness, 2) + pow(other.a - a, 2) + pow(other.b - b, 2)
  def difference(other: Color) = sqrt(differenceSqr(other))

  override def compare(other: Color): Int = differenceSqr(other) match {
    case 0 => 0
    case x if x > 0 => 1
    case _ => -1
  }

  private def minComponent = min(min(red, green), blue)
  private def maxComponent = max(max(red, green), blue)
}

object Color {
  def apply(a: Int, r: Int, g: Int, b: Int) = Color((a & 0xff) << 24 | (r & 0xff) << 16 | (g & 0xff) << 8 | (b & 0xff))
  def apply(a: Double, r: Double, g: Double, b: Double) = Color((a * 255).toInt, (r * 255).toInt, (g * 255).toInt, (b * 255).toInt)

  implicit class IntWrapper(val i: Int) extends AnyVal {
    def toColor = Color(i)
  }
}
