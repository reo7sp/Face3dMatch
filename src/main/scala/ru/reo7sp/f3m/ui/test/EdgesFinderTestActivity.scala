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

package ru.reo7sp.f3m.ui.test

import android.graphics.drawable.BitmapDrawable
import android.graphics.{BitmapFactory, Canvas, Paint}
import android.view.SurfaceView
import org.scaloid.common._
import ru.reo7sp.f3m.R
import ru.reo7sp.f3m.image.AndroidImage
import ru.reo7sp.f3m.image.AndroidImage._
import ru.reo7sp.f3m.image.edit.filter._
import ru.reo7sp.f3m.image.understand.content._
import ru.reo7sp.f3m.math.geometry.{Point, Size}

class EdgesFinderTestActivity extends SActivity {
  onCreate {
    setContentView(R.layout.edgesfindertestactivity)

    val surfaceView = find[SurfaceView](R.id.surfaceView)
    var bitmapDrawable = new BitmapDrawable(getResources, BitmapFactory.decodeResource(getResources, R.drawable.lena))
    surfaceView.setBackground(bitmapDrawable)

    var image = new AndroidImage(bitmapDrawable.getBitmap)

    def transformBitmap(): Unit = {
      val scaledImage = image.copy(size = Size(256, 256 / image.size.aspectRatio))
      bitmapDrawable = new BitmapDrawable(getResources, scaledImage)
      surfaceView.setBackground(bitmapDrawable)

      image = contrasted(desaturated(scaledImage), value = 4)
    }

    def drawDots(dots: TraversableOnce[Point]): Unit = {
      val canvas = new Canvas(bitmapDrawable.getBitmap)
      val paint = new Paint()
      paint.setColor(android.graphics.Color.RED)
      dots.foreach { point =>
        canvas.drawCircle(point.x.toFloat, point.y.toFloat, 1f, paint)
      }
    }

    transformBitmap()
    drawDots(findEdges(image))
  }
}
