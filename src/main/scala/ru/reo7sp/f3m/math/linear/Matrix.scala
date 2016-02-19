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

package ru.reo7sp.f3m.math.linear

import ru.reo7sp.f3m.math.geometry.Size
import ru.reo7sp.f3m.math.linear.Matrix.MatrixElement

case class Matrix[T](size: Size, elements: Seq[MatrixElement[T]]) {
  require(size.area == elements.size)

  type Element = MatrixElement[T]

  def apply(i: Int, j: Int) = elements(i * width + j)

  def width = size.width
  def height = size.width

  def hasOnlyVars = elements.forall(_.isLeft)
  def hasOnlyConsts = elements.forall(_.isRight)

  def toMultidimensionalArray: Array[Array[Element]] = {
    (0 until height).map { case i =>
      val t = i * width
      (0 until width).map {
        case j => elements(t + j)
      }.toArray
    }.toArray
  }

  def det = if (size.width == size.height && size.width <= 3) {
    size.width match {
      case 0 => 0
      case 1 => apply(0, 0)
      case 2 => ???
      case 3 => ???
    }
  } else None
}

object Matrix {
  type MatrixElement[T] = Either[Var[T], T]

  implicit class ArrayOfArrayOfMatrixElementWrapper[T](arr: Array[Array[MatrixElement[T]]]) {
    def toMatrix = {
      val width = if (arr.nonEmpty) arr(0).length else 0
      val height = arr.length
      val elements = arr.flatten.map(Right(_))
      Matrix(Size(width, height), elements)
    }
  }

}
