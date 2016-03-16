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
    faceDetector.findFaces(bitmap, faces)
    faces.collect { case face if face != null =>
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
