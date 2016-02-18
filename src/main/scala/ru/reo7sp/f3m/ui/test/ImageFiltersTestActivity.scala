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

import android.graphics.BitmapFactory
import android.graphics.drawable.BitmapDrawable
import android.view.SurfaceView
import android.widget.{Button, EditText}
import org.scaloid.common._
import ru.reo7sp.f3m.R
import ru.reo7sp.f3m.image.AndroidImage
import ru.reo7sp.f3m.image.AndroidImage._
import ru.reo7sp.f3m.image.edit.filter._

class ImageFiltersTestActivity extends SActivity {
  onCreate {
    setContentView(R.layout.imagefilterstestactivity)

    val editText = find[EditText](R.id.editText)
    def getInputNumber = try {
      editText.getText.toString.toDouble
    } catch {
      case _: Throwable => 1.0
    }

    val surfaceView = find[SurfaceView](R.id.surfaceView)
    var bitmapDrawable = new BitmapDrawable(getResources, BitmapFactory.decodeResource(getResources, R.drawable.lena))
    surfaceView.setBackground(bitmapDrawable)

    find[Button](R.id.desaturateButton).onClick {
      val image = new AndroidImage(bitmapDrawable.getBitmap)
      bitmapDrawable = new BitmapDrawable(getResources, desaturated(image))
      surfaceView.setBackground(bitmapDrawable)
    }

    find[Button](R.id.contrastButton).onClick {
      val image = new AndroidImage(bitmapDrawable.getBitmap)
      bitmapDrawable = new BitmapDrawable(getResources, contrasted(image, by = getInputNumber))
      surfaceView.setBackground(bitmapDrawable)
    }

    find[Button](R.id.scaleButton).onClick {
      val image = new AndroidImage(bitmapDrawable.getBitmap)
      bitmapDrawable = new BitmapDrawable(getResources, image.copy(size = image.size * getInputNumber))
      surfaceView.setBackground(bitmapDrawable)
    }
  }
}
