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

package ru.reo7sp.f3m.image.understand.perspective

import ru.reo7sp.f3m.math.geometry.Point

import scala.collection.SetLike
import scala.collection.generic.GenericSetTemplate
import scala.collection.immutable.HashSet

case class PartialScenery(cameraPos: Point) extends HashSet[Point] with GenericSetTemplate[Point, PartialScenery] with SetLike[Point, PartialScenery]

object PartialScenery {

  implicit class TraversableOfPoint3DWrapper(c: TraversableOnce[Point]) {
    def toPartialScenery(cameraPos: Point): PartialScenery = new PartialScenery(cameraPos) ++ c
  }

}
