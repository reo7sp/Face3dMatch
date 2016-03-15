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

import scala.collection.mutable

case class Matrix[T](size: Size, elements: Seq[MatrixElement[T]]) {
  //  require(size.area == elements.size, s"${size.area} != ${elements.size} (${elements.mkString(" ")})")

  type Element = MatrixElement[T]

  def apply(i: Int, j: Int) = elements(i * width + j)
  def getVarAt(i: Int, j: Int) = apply(i, j).left.get
  def getValAt(i: Int, j: Int) = apply(i, j).right.get

  def transpose = {
    val newElements = new mutable.ListBuffer[Element]
    for (j <- 0 until height; i <- 0 until width) {
      newElements += this (i, j)
    }
    Matrix(size.transpose, newElements)
  }

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
}

object Matrix {
  type MatrixElement[T] = Either[Var[T], T]

  implicit class SeqOfSeqOfMatrixElementToMatrixWrapper[T](arr: Seq[Seq[MatrixElement[T]]]) {
    def toMatrix = {
      val width = if (arr.nonEmpty) arr.head.length else 0
      val height = arr.length
      val elements = arr.flatten.map(Right(_))
      Matrix(Size(width, height), elements)
    }
  }

  implicit class SeqOfMatrixElementToMatrixWrapper[T](arr: Seq[MatrixElement[T]]) {
    def toMatrix(size: Size) = Matrix(size, arr)
  }

}
