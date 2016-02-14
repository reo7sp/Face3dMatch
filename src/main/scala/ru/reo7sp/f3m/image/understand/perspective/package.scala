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

package ru.reo7sp.f3m.image.understand

import ru.reo7sp.f3m.image.understand.perspective.Scenery.TraversableOfPoint3DWrapper
import ru.reo7sp.f3m.math.geometry.Line

package object perspective {
  def buildScenery(partials: Iterable[PartialScenery]): Scenery = {
    val lines = partials.par.flatMap { partialScenery =>
      partialScenery.map(Line(partialScenery.cameraPos, _))
    }

    lines.flatMap { line =>
      lines.map((line, _))
    }.map { case (line1, line2) =>
      line1 findIntersection line2
    }.filter(_.nonEmpty).map(_.get).seq.toScenery
  }

  def howSimilarAreSceneries(first: Scenery, second: Scenery) = first.count(second.contains).toDouble / first.size
}
