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
import ru.reo7sp.f3m.math.geometry.{Point, Rect}

trait Image {
  def width: Int
  def height: Int

  def apply(p: Point): Color
  def update(p: Point, c: Color): Unit

  def copy(rect: Rect = Rect(Point.zero(2), Point(width, height)), scale: Double = 1.0): Image

  def rows: Iterator[Iterator[Pixel]] = (0 until height).iterator.map(y => new PixelIterator(new RowsPixelIteratorStrategy, Point(0, y)))
  def columns: Iterator[Iterator[Pixel]] = (0 until width).iterator.map(x => new PixelIterator(new ColumnsPixelIteratorStrategy, Point(x, 0)))
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
    override def isOk(point: Point): Boolean = point.y < height
    override def nextAfter(oldPoint: Point): Point = {
      if (oldPoint.x + 1 == width) {
        Point(0, oldPoint.y + 1)
      } else {
        Point(oldPoint.x + 1, oldPoint.y)
      }
    }
  }

  class RowsPixelIteratorStrategy extends PixelIteratorStrategy {
    override def isOk(point: Point): Boolean = point.x < width
    override def nextAfter(oldPoint: Point): Point = oldPoint.copy(x = oldPoint.x + 1)
  }

  class ColumnsPixelIteratorStrategy extends PixelIteratorStrategy {
    override def isOk(point: Point): Boolean = point.y < height
    override def nextAfter(oldPoint: Point): Point = oldPoint.copy(y = oldPoint.y + 1)
  }

}

object Image {
  def apply(width: Int, height: Int): Image = new AndroidImage(Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888))

  implicit class TraversableOfPixelWrapper(i: TraversableOnce[Pixel]) {
    def toImage: Image = {
      val (iter1, iter2) = i.toIterator.duplicate
      val (w, h) = iter1.foldLeft((0.0, 0.0)) { case ((maxX, maxY), Pixel(Point(x, y), _)) => (maxX max x, maxY max y) }
      val img = Image(w.toInt, h.toInt)
      iter2.foreach { case Pixel(point, color) => img(point) = color }
      img
    }
  }

}