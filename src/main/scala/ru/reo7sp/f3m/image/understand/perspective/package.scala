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

import android.util.Log
import ru.reo7sp.f3m.image.understand.perspective.Scenery.TraversableOfPoint3dToSceneryWrapper
import ru.reo7sp.f3m.math.geometry.Line

package object perspective {
  def buildScenery(partials: Traversable[PartialScenery]): Scenery = {
    val lines = partials.flatMap { partialScenery =>
      partialScenery.points.map(Line(partialScenery.cameraPos, _))
    }

    val pairsOfLines = lines.view.flatMap { line =>
      lines.map((_, line))
    }.filterNot { case (line1, line2) =>
      line1 == line2
    }.force


    pairsOfLines.par.map { case (line1, line2) =>
      Log.w("", s"$line1 <-> $line2")
      line1 findMinDistancePoint line2
    }.seq.toScenery
  }
}
