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

import android.content.Context
import android.hardware.Camera
import android.util.AttributeSet
import android.view.SurfaceHolder.Callback
import android.view.{SurfaceHolder, SurfaceView}
import org.scaloid.common._

import scala.collection.JavaConversions._
import scala.util.control.NonFatal

//noinspection ScalaDeprecation
class CameraPreview(val context: Context, val attrs: AttributeSet) extends SurfaceView(context, attrs) {
  private[this] var _camera: Camera = null
  private[this] val _surfaceHolder = getHolder

  def camera = _camera

  def camera_=(camera: Camera): Unit = {
    _camera = camera

    _surfaceHolder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS)
    _surfaceHolder.addCallback(MyCallback)
  }

  private object MyCallback extends Callback {
    private[this] var _isRunning = false

    private def startPreview(holder: SurfaceHolder): Unit = {
      def getBestPreviewSize(width: Int, height: Int, parameters: Camera#Parameters) = {
        var result: Camera#Size = null
        for (size <- parameters.getSupportedPreviewSizes) {
          if (size.width <= width && size.height <= height) {
            if (result == null) {
              result = size
            } else {
              val resultArea = result.width * result.height
              val newArea = size.width * size.height
              if (newArea > resultArea) {
                result = size
              }
            }
          }
        }
        Option(result)
      }

      try {
        if (_isRunning) {
          camera.stopPreview()
        } else {
          _isRunning = true
          val parameters = camera.getParameters
          val sizeOpt = getBestPreviewSize(getWidth, getHeight, parameters)
          sizeOpt foreach { size =>
            parameters.setPreviewSize(size.height, size.width)
            camera.setParameters(parameters)
            camera.setDisplayOrientation(90)
          }
        }
        camera.setPreviewDisplay(holder)
        camera.startPreview()
      } catch {
        case e: IOException => error(e.toString)(LoggerTag("CameraPreview"))
      }
    }

    override def surfaceCreated(holder: SurfaceHolder): Unit = startPreview(holder)

    override def surfaceChanged(holder: SurfaceHolder, format: Int, width: Int, height: Int): Unit = if (holder.getSurface != null) {
      try {
        camera.stopPreview()
      } catch {
        case NonFatal(e) => ()
      }
      startPreview(holder)
      }

    override def surfaceDestroyed(holder: SurfaceHolder): Unit = ()
  }
}
