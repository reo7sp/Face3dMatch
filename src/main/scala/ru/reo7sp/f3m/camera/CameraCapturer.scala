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

import android.graphics.BitmapFactory
import android.hardware.Camera
import android.hardware.Camera.{Face, FaceDetectionListener, PictureCallback}
import ru.reo7sp.f3m.image.AndroidImage
import ru.reo7sp.f3m.math.geometry.{Point, Rect}

import scala.concurrent.Promise

//noinspection ScalaDeprecation
class CameraCapturer(val camera: Camera) {
  require(camera != null)

  def setupFaceDetection(): Unit = camera.setFaceDetectionListener(MyFaceListener)

  def capture() = {
    val promise = Promise[AndroidImage]
    camera.takePicture(null, null, new PictureCallback {
      override def onPictureTaken(data: Array[Byte], camera: Camera): Unit = {
        promise.success(new AndroidImage(BitmapFactory.decodeByteArray(data, 0, data.length)))
      }
    })
    promise.future
  }

  def captureFace() = {
    val promise = Promise[AndroidImage]
    MyFaceListener.onFace { face =>
      camera.takePicture(null, null, new PictureCallback {
        override def onPictureTaken(data: Array[Byte], camera: Camera): Unit = {
          val rect = Rect(Point(face.rect.left, face.rect.top), Point(face.rect.right, face.rect.bottom))
          promise.success(new AndroidImage(BitmapFactory.decodeByteArray(data, 0, data.length)).copy(rect))
        }
      })
    }
    promise.future
  }

  private object MyFaceListener extends FaceDetectionListener {
    private[this] var _callback: Face => Any = null

    override def onFaceDetection(faces: Array[Face], camera: Camera): Unit = if (_callback != null && faces.nonEmpty) {
      _callback(faces(0))
      _callback = null
    }

    def onFace(callback: Face => Any): Unit = _callback = callback
  }
}
