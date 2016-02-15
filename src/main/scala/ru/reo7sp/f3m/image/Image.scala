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

import ru.reo7sp.f3m.math.geometry.{Point, Size}

trait Image {
  def size: Size

  def apply(p: Point): Color

  def rows: Iterator[Iterator[Pixel]] = (0 until size.height).iterator.map(y => new PixelIterator(new RowsPixelIteratorStrategy, Point(0, y)))
  def columns: Iterator[Iterator[Pixel]] = (0 until size.width).iterator.map(x => new PixelIterator(new ColumnsPixelIteratorStrategy, Point(x, 0)))
  def pixels: Iterator[Pixel] = new PixelIterator(new AllPixelIteratorStrategy)

  class PixelIterator(strategy: PixelIteratorStrategy, var point: Point = Point.zero(2)) extends Iterator[Pixel] {
    override def hasNext: Boolean = strategy.isOk(point)
    override def next(): Pixel = {
      val r = Pixel(point, apply(point))
      point = strategy.nextAfter(point)
      r
    }
  }

  trait PixelIteratorStrategy {
    def isOk(point: Point): Boolean
    def nextAfter(oldPoint: Point): Point
  }

  class AllPixelIteratorStrategy extends PixelIteratorStrategy {
    override def isOk(point: Point): Boolean = point.y < size.height
    override def nextAfter(oldPoint: Point): Point = {
      if (oldPoint.x + 1 == size.width) {
        Point(0, oldPoint.y + 1)
      } else {
        Point(oldPoint.x + 1, oldPoint.y)
      }
    }
  }

  class RowsPixelIteratorStrategy extends PixelIteratorStrategy {
    override def isOk(point: Point): Boolean = point.x < size.width
    override def nextAfter(oldPoint: Point): Point = oldPoint.copy(x = oldPoint.x + 1)
  }

  class ColumnsPixelIteratorStrategy extends PixelIteratorStrategy {
    override def isOk(point: Point): Boolean = point.y < size.height
    override def nextAfter(oldPoint: Point): Point = oldPoint.copy(y = oldPoint.y + 1)
  }

}

object Image {
  implicit class TraversableOfPixelWrapper(pixels: TraversableOnce[Pixel]) {
    def toImage[T <: Image](implicit companion: ImageCompanion[T]) = companion(pixels)
    def toImage[T <: Image](size: Size)(implicit companion: ImageCompanion[T]) = companion(pixels, size)
  }
}