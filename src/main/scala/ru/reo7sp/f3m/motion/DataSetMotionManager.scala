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

package ru.reo7sp.f3m.motion

import android.hardware.SensorManager
import ru.reo7sp.f3m.math.NumExtensions.DoubleExtensions
import ru.reo7sp.f3m.math.geometry.Point

class DataSetMotionManager(override val sensorManager: SensorManager, val data: Traversable[(Point, Long)]) extends MotionManager(sensorManager) { // HACK
  private[this] val _dataIterator = data.toIterator
  private[this] var _lastTime = 0L

  override def start(delay: Int): Unit = {
    _dataIterator.foreach { case (point, time) =>
      if (_lastTime != 0) {
        val dt = (time - _lastTime) * 1.0e-9
        _x += point(0) * dt.squared / 2
        _y += point(1) * dt.squared / 2
        _z += point(2) * dt.squared / 2

        if (_listeners.nonEmpty) {
          val p = position
          _listeners.foreach(_.apply(p))
        }
      }
      _lastTime = time
    }
  }

  override def stop(): Unit = ()
}
