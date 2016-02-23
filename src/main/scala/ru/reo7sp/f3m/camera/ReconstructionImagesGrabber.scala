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

import android.graphics.PointF
import android.media.FaceDetector
import ru.reo7sp.f3m.image.ArrayImage.{ImageToArrayImageWrapper, TraversableOfPixelToImageTWrapper, imageCompanion}
import ru.reo7sp.f3m.image.Pixel
import ru.reo7sp.f3m.image.edit.filter._
import ru.reo7sp.f3m.image.understand.content._
import ru.reo7sp.f3m.image.understand.perspective.PartialScenery.TraversableOfPoint3dToPartialSceneryWrapper
import ru.reo7sp.f3m.image.understand.perspective.{PartialScenery, _}
import ru.reo7sp.f3m.math.geometry.{Point, Rect, Size}
import ru.reo7sp.f3m.motion.MotionManager
import ru.reo7sp.f3m.util.AndroidExecutionContext.executionContext

import scala.collection.mutable
import scala.concurrent.Future

class ReconstructionImagesGrabber(_cameraCapturer: CameraCapturer, _motionManager: MotionManager) {
  require(_cameraCapturer != null)
  require(_motionManager != null)

  val partialSceneries = new mutable.ListBuffer[PartialScenery]

  def startGrabbing(): Unit = {
    _motionManager.start()
    _motionManager.onMotion { position =>
      _cameraCapturer.capture().onSuccess { case image =>
        def acquireDistance = {
          val a = Array(0f, 0f, 0f)
          _cameraCapturer.camera.getParameters.getFocusDistances(a)
          a(1)
        }

        Future {
          val scaledImage = image.copy(size = Size(64, 64 / image.size.aspectRatio))

          val faceDetector = new FaceDetector(scaledImage.size.width, scaledImage.size.height, 1)
          val faces = new Array[FaceDetector#Face](1)
          faceDetector.findFaces(scaledImage, faces)
          val face = faces.head
          val faceMidPoint = new PointF()
          face.getMidPoint(faceMidPoint)
          val faceWidth = face.eyesDistance()
          val faceRect = Rect(
            Point(faceMidPoint.x - faceWidth, faceMidPoint.y - faceWidth),
            Point(faceMidPoint.x + faceWidth, faceMidPoint.y + faceWidth)
          )

          val filteredImage = scaledImage.toArrayImage.pixels.filter { case Pixel(point, _) => faceRect has point }.toImage(scaledImage.size)
          val croppedImage = filteredImage.copy(Rect(faceRect.topLeft.copy(x = 0), faceRect.bottomRight.copy(x = faceRect.width)))
          val editedImage = contrasted(desaturated(croppedImage), factor = 4)

          val zOffset = Point(0, 0, acquireDistance)
          val edges = findEdges(editedImage, edgeThreshold = 0.1).map(_ + position + zOffset)

          partialSceneries.synchronized {
            partialSceneries += edges.toPartialScenery(cameraPos = position)
          }
        }
      }
    }
  }

  def stopGrabbing(): Unit = _motionManager.stop()

  def compute = buildScenery(partialSceneries)
}
