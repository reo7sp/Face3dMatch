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

import ru.reo7sp.f3m.math.linear.Matrix.MatrixElement

import scala.collection.SeqLike
import scala.collection.generic.GenericTraversableTemplate

case class Matrix[T](width: Int, height: Int, elements: Seq[MatrixElement]) extends Seq[MatrixElement]
  with GenericTraversableTemplate[MatrixElement, Matrix[T]]
  with SeqLike[MatrixElement, Matrix[T]] {

  require(width * height == elements.size)

  def apply(i: Int, j: Int) = elements(i * width + j)

  def hasOnlyVars = forall(_.isLeft)
  def hasOnlyConsts = forall(_.isRight)

  override def iterator = elements.iterator
  override def length: Int = elements.length
}

object Matrix {
  type MatrixElement[T] = Either[Variable, T]

  def apply[T](width: Int, height: Int, elements: Seq[MatrixElement], fillWith: T) = {
    new Matrix(width, height, elements ++ Seq.fill((width * height - elements.size) max 0)(Right(fillWith)))
  }
}
