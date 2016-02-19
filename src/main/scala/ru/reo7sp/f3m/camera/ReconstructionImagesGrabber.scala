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

import ru.reo7sp.f3m.image.ArrayImage._
import ru.reo7sp.f3m.image.Pixel
import ru.reo7sp.f3m.image.edit.filter._
import ru.reo7sp.f3m.image.understand.content._
import ru.reo7sp.f3m.image.understand.perspective.PartialScenery.TraversableOfPoint3DWrapper
import ru.reo7sp.f3m.image.understand.perspective.{PartialScenery, _}
import ru.reo7sp.f3m.math.geometry.Size
import ru.reo7sp.f3m.motion.MotionManager

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ReconstructionImagesGrabber(_cameraCapturer: CameraCapturer, _motionManager: MotionManager) {
  require(_cameraCapturer != null)
  require(_motionManager != null)

  val partialSceneries = new mutable.ListBuffer[PartialScenery]

  def startGrabbing(): Unit = {
    _cameraCapturer.setupFaceDetection()
    _motionManager.start()
    _motionManager.onMotion { position =>
      _cameraCapturer.captureWithFace().onSuccess { case (image, face) =>
        Future {
          val scaledImage = image.copy(size = Size(64, 64 / image.size.aspectRatio)).toArrayImage
          val filteredImage = scaledImage.pixels.filter { case Pixel(point, _) =>
            face.rect.contains(point.x.toInt, point.y.toInt)
          }.toImage(scaledImage.size)
          val editedImage = contrasted(desaturated(filteredImage), value = 4)

          partialSceneries.synchronized {
            partialSceneries += findEdges(editedImage).toPartialScenery(cameraPos = position)
          }
        }
      }
    }
  }

  def stopGrabbing(): Unit = _motionManager.stop()

  def compute = buildScenery(partialSceneries)
}
