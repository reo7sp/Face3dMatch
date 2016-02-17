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

package ru.reo7sp.f3m.ui

import android.hardware.Camera
import android.hardware.Camera.CameraInfo
import android.view.View
import android.widget.Button
import org.scaloid.common._
import ru.reo7sp.f3m.R
import ru.reo7sp.f3m.camera.{CameraCapturer, CameraPreview, ReconstructionImagesGrabber}
import ru.reo7sp.f3m.image.understand.perspective.Scenery
import ru.reo7sp.f3m.motion.MotionManager

import scala.collection.mutable
import scala.util.Random
import scala.util.control.NonFatal

//noinspection ScalaDeprecation
class CapturingActivity extends SActivity {
  private[this] var _camera: Camera = null
  private[this] var _motionManager: MotionManager = null
  private[this] var _grabber: ReconstructionImagesGrabber = null
  private[this] var _callbackId: Int = 0 // HACK

  onCreate {
    _camera = acquireCamera()
    _motionManager = new MotionManager(sensorManager)
    _grabber = new ReconstructionImagesGrabber(new CameraCapturer(_camera), _motionManager)
    _callbackId = getIntent.getIntExtra("callbackId", 0) // HACK

    setContentView(R.layout.capturingactivity)
    find[CameraPreview](R.id.cameraPreview).camera = _camera
    find[Button](R.id.cancelButton).onClick {
      CapturingActivity._actionsQueue.remove(_callbackId) // HACK
      finish()
    }
    find[Button](R.id.startButton).onClick {
      _grabber.startGrabbing()
      onDestroy(_grabber.stopGrabbing())

      find[Button](R.id.startButton).setVisibility(View.GONE)
      find[Button](R.id.stopButton).setVisibility(View.VISIBLE)
    }
    find[Button](R.id.stopButton).onClick {
      val callback = CapturingActivity._actionsQueue.remove(_callbackId).get // HACK
      _grabber.stopGrabbing()
      callback(_grabber.compute)
      finish()
    }
  }

  private def acquireCamera(): Camera = {
    def getIdOfFrontCamera = {
      (0 until Camera.getNumberOfCameras).find { id =>
        val info = new CameraInfo
        Camera.getCameraInfo(id, info)
        info.facing == CameraInfo.CAMERA_FACING_FRONT
      }.getOrElse(0)
    }

    try {
      val camera = Camera.open(getIdOfFrontCamera)
      onDestroy(camera.release())
      camera
    } catch {
      case NonFatal(e) =>
        error(e.toString)
        alert("No camera!", e.toString)
        throw e
    }
  }
}

object CapturingActivity {
  private val _actionsQueue = new mutable.HashMap[Int, Scenery => Any] // HACK

  def queueAction(action: Scenery => Any) = {
    // HACK
    val id = Random.nextInt()
    _actionsQueue(id) = action
    id
  }
}
