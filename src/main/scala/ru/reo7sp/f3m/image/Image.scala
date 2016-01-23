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

trait Image extends Iterable[Pixel] {
  def width: Int
  def height: Int

  def apply(p: Point): Color

  def copy(rect: Rect): Image

  def lines = new Iterable[Image] {
    override def iterator: Iterator[Image] = new Iterator[Image] {
      private[this] var _y = 1

      override def hasNext: Boolean = _y < height

      override def next(): Image = {
        val r = copy(Rect(Point(0, _y - 1), Point(width, _y)))
        _y += 1
        r
      }
    }
  }

  override def iterator: Iterator[Pixel] = new Iterator[Pixel] {
    private[this] var _x, _y = 0

    override def hasNext = _y < height

    override def next() = {
      val p = Point(_x, _y)
      val c = apply(p)
      _x += 1
      if (_x == width) {
        _x = 0
        _y += 1
      }
      Pixel(p, c)
    }
  }
}
