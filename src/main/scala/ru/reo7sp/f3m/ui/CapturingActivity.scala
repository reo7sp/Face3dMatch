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
import org.scaloid.common._
import ru.reo7sp.f3m.camera.{CameraCapturer, CameraPreview, ReconstructionImagesGrabber}
import ru.reo7sp.f3m.image.understand.perspective.Scenery
import ru.reo7sp.f3m.motion.MotionManager
import ru.reo7sp.f3m.ui.CapturingActivity.FunctionWrapper

import scala.util.control.NonFatal

class CapturingActivity extends SActivity {
  private[this] val _camera = acquireCamera()
  private[this] val _motionManager = new MotionManager(sensorManager)
  private[this] val _grabber = new ReconstructionImagesGrabber(new CameraCapturer(_camera), _motionManager)
  private[this] val _callback = getIntent.getSerializableExtra("callback").asInstanceOf[FunctionWrapper]

  onCreate {
    contentView = new SVerticalLayout {
      STextView("Перемещайте телефон вдоль одной линии")
      new CameraPreview(_camera).here
      new SLinearLayout {
        SButton("Отмена", finish())
        SButton("Готово", {
          _grabber.stopGrabbing()
          _callback(_grabber.compute)
          finish()
        })
      }.wrap.here
    }
    _grabber.startGrabbing()
    onDestroy(_grabber.stopGrabbing())
  }

  private def acquireCamera(): Camera = {
    try { {
      val camera = Camera.open(1)
      onDestroy(camera.release())
      camera
    }
    } catch {
      case NonFatal(e) =>
        error(e.toString)
        alert("No camera!", e.toString)
        throw e
    }
  }
}

object CapturingActivity {

  implicit class FunctionWrapper(c: Scenery => Any) extends Serializable {
    def apply(scenery: Scenery) = c(scenery)
  }

}
