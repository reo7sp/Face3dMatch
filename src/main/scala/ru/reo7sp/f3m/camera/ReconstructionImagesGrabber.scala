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

package ru.reo7sp.f3m.camera

import android.content.Context
import android.hardware.SensorManager
import org.scaloid.common._
import ru.reo7sp.f3m.image.ArrayImage.ImageToArrayImageWrapper
import ru.reo7sp.f3m.image.edit.filter._
import ru.reo7sp.f3m.image.understand.content._
import ru.reo7sp.f3m.image.understand.perspective.PartialScenery.TraversableOfPoint3dToPartialSceneryWrapper
import ru.reo7sp.f3m.image.understand.perspective.{PartialScenery, _}
import ru.reo7sp.f3m.math.geometry.{Point, Size}
import ru.reo7sp.f3m.motion.MotionManager
import ru.reo7sp.f3m.util.AndroidExecutionContext.executionContext

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.Random

class ReconstructionImagesGrabber(_cameraCapturer: CameraCapturer, _motionManager: MotionManager)(implicit ctx: Context) {
  require(_cameraCapturer != null)
  require(_motionManager != null)

  val partialSceneries = new mutable.ListBuffer[PartialScenery]

  private implicit val _loggerTag = LoggerTag("ReconstructionImagesGrabber")

  def startGrabbing(): Unit = {
    _motionManager.start(SensorManager.SENSOR_DELAY_NORMAL)
    _motionManager.onMotion { position =>
      _cameraCapturer.capture().onSuccess { case image =>
        def acquireDistance = {
          val a = Array(0f, 0f, 0f)
          _cameraCapturer.camera.getParameters.getFocusDistances(a)
          a(1)
        }

        Future {
          val faceRect = try {
            val f = Future {
              findFaces(image).headOption.getOrElse(image.size.toRect)
            }
            Await.result(f, 2 seconds)
          } catch {
            case e: Throwable =>
              warn(e.toString)
              image.size.toRect
          }

          val imageOnlyWithFace = image.copy(faceRect, Size(32, 32 / image.size.aspectRatio))
          val editedImage = contrasted(desaturated(imageOnlyWithFace.toArrayImage), factor = 4)

          val zOffset = Point(0, 0, acquireDistance)
          val edges = findEdges(editedImage /*, edgeThreshold = 0.1*/).map { p =>
            p + position + zOffset
          }.filter { p =>
            (1 to 4).view.map(x => Random.nextBoolean()).reduce(_ && _)
          }

          partialSceneries.synchronized {
            partialSceneries += edges.toPartialScenery(cameraPos = position)
          }
        }
      }
    }
  }

  def stopGrabbing(): Unit = _motionManager.stop()

  def compute = partialSceneries.synchronized(buildScenery(partialSceneries))
}
