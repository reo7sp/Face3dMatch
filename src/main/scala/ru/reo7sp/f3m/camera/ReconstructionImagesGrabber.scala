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

package ru.reo7sp.f3m.camera

import ru.reo7sp.f3m.image.ArrayImage.AndroidImageWrapper
import ru.reo7sp.f3m.image.edit.filter._
import ru.reo7sp.f3m.image.understand.content._
import ru.reo7sp.f3m.image.understand.perspective.PartialScenery.TraversableOfPoint3DWrapper
import ru.reo7sp.f3m.image.understand.perspective.{PartialScenery, _}
import ru.reo7sp.f3m.motion.MotionManager

import scala.collection.mutable
import scala.concurrent.Future

class ReconstructionImagesGrabber(_cameraCapturer: CameraCapturer, _motionManager: MotionManager) {
  val partialSceneries = new mutable.ListBuffer[PartialScenery]

  def startGrabbing(): Unit = {
    _motionManager.start()
    _motionManager.onMotion { position =>
      _cameraCapturer.capture().onSuccess { case image =>
        Future {
          val scaledImage = image.copy(scale = 0.01).toArrayImage
          val editedImage = contrasted(desaturated(scaledImage), by = 10)
          partialSceneries.synchronized {
            partialSceneries += findEdges(editedImage).toPartialScenery(position)
          }
        }
      }
    }
  }

  def stopGrabbing(): Unit = _motionManager.stop()

  def compute = buildScenery(partialSceneries)
}
