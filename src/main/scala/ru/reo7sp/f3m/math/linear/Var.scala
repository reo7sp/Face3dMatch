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

case class Var[T](name: Symbol, var value: Option[T] = None) extends Iterable[T] {
  //  private val _operations = new Queue[T => T]

  def get = if (value.isDefined) {
    var result: T = value.get
    //    _operations.foreach(op => result = op(result))
    result
  } else None

  def set(t: T) = value = Option(t)
  def unset() = value = None

  def modifyWith(op: T => T) = {
    ???
  }

  def |(op: T => T) = this modifyWith op

  def clearOperations() = Var(name, value)

  override def iterator = new Iterator[T] {
    override def hasNext = value.nonEmpty
    override def next() = value.get
  }
}
