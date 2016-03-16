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

package ru.reo7sp.f3m.image.understand.content

import android.graphics.PointF
import android.media.FaceDetector
import ru.reo7sp.f3m.image.AndroidImage.ImageToAndroidImageWrapper
import ru.reo7sp.f3m.image.Image
import ru.reo7sp.f3m.math.geometry.{Point, Rect}

object AndroidFaceFinder extends FaceFinder {
  override def apply(image: Image): Traversable[Rect] = {
    val bitmap = image.toAndroidImage.handle
    val faceDetector = new FaceDetector(bitmap.getWidth, bitmap.getHeight, 1)
    val faces = new Array[FaceDetector#Face](1)
    val facesCount = faceDetector.findFaces(bitmap, faces)
    faces.take(facesCount).collect { case face if face != null =>
      val faceMidPoint = new PointF()
      face.getMidPoint(faceMidPoint)
      val faceWidth = face.eyesDistance()
      Rect(
        Point(faceMidPoint.x - faceWidth, faceMidPoint.y - faceWidth),
        Point(faceMidPoint.x + faceWidth, faceMidPoint.y + faceWidth)
      )
    }
  }
}
