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

import scala.collection.immutable.Queue

case class Var[T](name: Symbol, value: Option[T] = None, operations: Queue[T => T] = Queue[T => T]()) extends Iterable[T] {
  def compute = if (value.isDefined) {
    var result = value.get
    operations.foreach(op => result = op(result))
    copy(value = Some(result), operations = Queue[T => T]())
  } else this

  def get = compute.value.get

  def modifyWith(op: T => T) = copy(operations = operations enqueue op)
  def |(op: T => T) = this modifyWith op

  def clear = copy(operations = Queue[T => T]())

  override def iterator = new Iterator[T] {
    override def hasNext = value.nonEmpty
    override def next() = value.get
  }
}
