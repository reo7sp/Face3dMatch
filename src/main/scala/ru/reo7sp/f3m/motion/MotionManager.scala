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

package ru.reo7sp.f3m.motion

import android.hardware.{Sensor, SensorEvent, SensorEventListener, SensorManager}
import ru.reo7sp.f3m.math.NumExtensions.DoubleExtensions
import ru.reo7sp.f3m.math.geometry.Point

import scala.collection.mutable

class MotionManager(val sensorManager: SensorManager) {
  require(sensorManager != null)

  private[this] var _x, _y, _z = 0.0
  private[this] var _listeners = new mutable.ListBuffer[Point => Any]
  private[this] var _isRunning = false

  def start(delay: Int = SensorManager.SENSOR_DELAY_GAME): Unit = if (!_isRunning) {
    _isRunning = true
    sensorManager.registerListener(MySensorListener, sensorManager.getDefaultSensor(Sensor.TYPE_LINEAR_ACCELERATION), delay)
  }

  def stop(): Unit = if (_isRunning) {
    _isRunning = false
    sensorManager.unregisterListener(MySensorListener)
    reset()
  }

  def position = Point(_x, _y, _z)

  def reset(): Unit = {
    MySensorListener.reset()
    _listeners.clear()
    _x = 0
    _y = 0
    _z = 0
  }

  def onMotion(callback: Point => Any) = _listeners += callback
  def unregister(callback: Point => Any) = _listeners -= callback

  private object MySensorListener extends SensorEventListener {
    private[this] var _lastTime = 0L

    override def onSensorChanged(event: SensorEvent): Unit = {
      if (_lastTime != 0) {
        val dt = (event.timestamp - _lastTime) * 1.0e-9
        _x += event.values(0) * dt.squared / 2
        _y += event.values(1) * dt.squared / 2
        _z += event.values(2) * dt.squared / 2

        if (_listeners.nonEmpty) {
          val p = position
          _listeners.foreach(_.apply(p))
        }
      }
      _lastTime = event.timestamp
    }

    override def onAccuracyChanged(sensor: Sensor, accuracy: Int): Unit = ()

    def reset(): Unit = _lastTime = 0
  }
}
