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

import java.io.IOException

import android.hardware.Camera
import android.view.SurfaceHolder
import android.view.SurfaceHolder.Callback
import org.scaloid.common.{SSurfaceView, _}

import scala.util.control.NonFatal

//noinspection ScalaDeprecation
class CameraPreview(val camera: Camera)(implicit context: android.content.Context, parentVGroup: TraitViewGroup[_] = null) extends SSurfaceView {
  private[this] val _surfaceHolder = getHolder

  _surfaceHolder.addCallback(new Callback {
    private[this] var _isRunning = false

    private def startPreview(holder: SurfaceHolder): Unit = {
      try { {
        if (_isRunning) {
          camera.stopPreview()
        } else {
          _isRunning = true
        }
        camera.setPreviewDisplay(holder)
        camera.startPreview()
      }
      } catch {
        case e: IOException => error(e.toString)(LoggerTag("CameraPreview"))
      }
    }

    override def surfaceCreated(holder: SurfaceHolder): Unit = startPreview(holder)

    override def surfaceChanged(holder: SurfaceHolder, format: Int, width: Int, height: Int): Unit = if (holder.getSurface != null) {
      try { {
        camera.stopPreview()
      }
      } catch {
        case NonFatal(e) => ()
      }
      startPreview(holder)
    }

    override def surfaceDestroyed(holder: SurfaceHolder): Unit = ()
  })
}
