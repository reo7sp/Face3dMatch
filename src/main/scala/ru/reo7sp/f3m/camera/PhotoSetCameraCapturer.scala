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

package ru.reo7sp.f3m.camera

import android.content.Context
import android.hardware.Camera
import ru.reo7sp.f3m.image.AndroidImage

import scala.concurrent.{Future, Promise}

class PhotoSetCameraCapturer(override val camera: Camera, val images: TraversableOnce[AndroidImage])(implicit ctx: Context) extends CameraCapturer(camera) { // HACK
  private[this] val _imagesIterator = images.toIterator

  override def capture(): Future[AndroidImage] = {
    val promise = Promise[AndroidImage]
    if (_imagesIterator.hasNext) {
      promise.success(_imagesIterator.next())
    } else {
      promise.failure(new ArrayIndexOutOfBoundsException)
    }
    promise.future
  }
}
