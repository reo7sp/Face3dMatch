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
import ru.reo7sp.f3m.camera.{CameraCapturer, CameraPreview}

import scala.util.control.NonFatal

class MainActivity extends SActivity {
  private[this] val _camera = acquireCamera()
  private[this] val _capturer = new CameraCapturer(_camera)

  onCreate {
    contentView = new SVerticalLayout {
      new CameraPreview(_camera).here
      new SLinearLayout {
        SButton("Setup", _capturer.capture().onSuccess(???))
        SButton("Unlock", _capturer.capture().onSuccess(???))
      }.wrap.here
    }
  }

  private def acquireCamera(): Camera = {
    try { {
      val camera = Camera.open()
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
