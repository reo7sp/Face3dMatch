/*
 * Copyright 2016 Oleg Morozenkov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ru.reo7sp.f3m.image.understand.perspective

import ru.reo7sp.f3m.math.NumExtensions._
import ru.reo7sp.f3m.math.geometry.Point

case class Scenery(points: Set[Point]) {
  def similarityWith(other: Scenery, threshold: Double = 16) = {
    val thresholdSqr = threshold.squared
    points.par.count(p1 => other.points.exists(p2 => (p1 distanceSqr p2) <= thresholdSqr)).toDouble / points.size
  }
}

object Scenery {
  def apply(c: TraversableOnce[Point]): Scenery = Scenery(c.toSet)

  implicit class TraversableOfPoint3dToSceneryWrapper(c: TraversableOnce[Point]) {
    def toScenery = Scenery(c)
  }

}
