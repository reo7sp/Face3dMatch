package ru.reo7sp.f3m.image.understand.content

/*
import org.opencv.core.{CvType, Mat, MatOfRect}
import org.opencv.objdetect.CascadeClassifier
import ru.reo7sp.f3m.image.AndroidImage.ImageToAndroidImageWrapper
import ru.reo7sp.f3m.image.{Image, Pixel}
import ru.reo7sp.f3m.math.geometry.{Point, Rect}

object OpenCvFaceFinder extends FaceFinder {
  override def apply(image: Image): Traversable[Rect] = {
    val bitmap = image.toAndroidImage.handle
    val matImage = new Mat(bitmap.getHeight, bitmap.getWidth, CvType.CV_8UC1)
    image.pixels.foreach { case Pixel(point, color) =>
      matImage.put(point.y.toInt, point.x.toInt, Array(color.redInt, color.greenInt, color.blueInt))
    }

    val faceDetector = new CascadeClassifier(getClass.getResource("/lbpcascade_frontalface.xml").getPath)
    val faceDetections = new MatOfRect()
    faceDetector.detectMultiScale(matImage, faceDetections)

    faceDetections.toArray.map { rect =>
      Rect(Point(rect.x, rect.y), Point(rect.x + rect.width, rect.y + rect.height))
    }
  }
}
*/
