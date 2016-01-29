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

import scala.collection.SeqLike
import scala.collection.generic.GenericTraversableTemplate

case class Matrix[T](size: Size, elements: Seq[MatrixElement[T]]) extends Seq[MatrixElement[T]]
  with GenericTraversableTemplate[MatrixElement[T], Matrix[T]]
  with SeqLike[MatrixElement[T], Matrix[T]] {

  require(size.area == elements.size)

  def apply(i: Int, j: Int) = elements(i * width + j)

  def width = size.width
  def height = size.width

  def hasOnlyVars = forall(_.isLeft)
  def hasOnlyConsts = forall(_.isRight)

  def toMultidimensionalArray: Array[Array[MatrixElement[T]]] = {
    (0 until height).map {
      case i => {
        val t = i * width
        (0 until width).map {
          case j => elements(t + j)
        }.toArray
      }
    }.toArray
  }

  override def iterator = elements.iterator
  override def length = elements.length
}

object Matrix {
  type MatrixElement[T] = Either[Var, T]

  def apply[T](size: Size, elements: Seq[T]) = Matrix(size, elements.map(Right(_)))
  def apply[T](size: Size, fillWith: Seq[Var[T]]) = Matrix(size, Seq.fill(size.area)(Left(fillWith)))
  def apply[T](size: Size, fillWith: T) = Matrix(size, Seq.fill(size.area)(Right(fillWith)))

  implicit class ArrayOfArrayOfMatrixElementWrapper[T](arr: Array[Array[MatrixElement[T]]]) {
    def toMatrix = Matrix(Size(if (arr.nonEmpty) arr(0).length else 0, arr.length), arr.flatten)
  }
}
