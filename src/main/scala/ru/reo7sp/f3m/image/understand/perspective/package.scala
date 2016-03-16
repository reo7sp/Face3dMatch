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

import ru.reo7sp.f3m.image.understand.perspective.Scenery.TraversableOfPoint3dToSceneryWrapper
import ru.reo7sp.f3m.math.geometry.Line
import ru.reo7sp.f3m.util.AndroidExecutionContext.executionContext

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Random

package object perspective {
  def buildScenery(partials: Traversable[PartialScenery]): (Future[Scenery], ProgressTeller) = {
    val progressTeller = new ProgressTeller
    val future = Future {
      val lines = partials.par.flatMap { partialScenery =>
        partialScenery.points.view.map(Line(partialScenery.cameraPos, _))
      }.toStream.view

      def hasToDo = (1 to 8).view.map(x => Random.nextBoolean()).reduce(_ && _)

      var progress = 0
      lines.combinations(2).filter(x => hasToDo).collect {
        case c if c.head.initialPoint != c.last.initialPoint && (c.head findMinDistance c.last) < 0.5 =>
          progress += 1
          progressTeller set progress
          c.head findMinDistancePoint c.last
      }.toScenery
    }
    (future, progressTeller)
  }

  class ProgressTeller {
    private[this] val _listeners = new mutable.ListBuffer[Int => Any]

    def onUpdate(listener: Int => Any): Unit = _listeners += listener

    def set(value: Int) = _listeners.foreach(_(value))
  }
}
