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
import ru.reo7sp.f3m.math.NumExtensions.DoubleWrapper
import ru.reo7sp.f3m.math.geometry.Point

import scala.collection.mutable

class MotionManager(val sensorManager: SensorManager) {
  private[this] var _x, _y, _z = 0.0
  private[this] var _listeners = new mutable.ListBuffer[Point => ()]

  private[this] val _listener = new SensorEventListener {
    private[this] var _lastTime = 0L

    override def onSensorChanged(event: SensorEvent): Unit = {
      if (_lastTime != 0) {
        val dt = (event.timestamp - _lastTime) * 1.0e9
        _x += event.values(0) * dt.squared / 2
        _y += event.values(1) * dt.squared / 2
        _z += event.values(2) * dt.squared / 2

        if (_listeners.nonEmpty) {
          val p = position
          _listeners.par.foreach(_ (p))
        }
      }
      _lastTime = event.timestamp
    }

    override def onAccuracyChanged(sensor: Sensor, accuracy: Int): Unit = ()

    def reset(): Unit = _lastTime = 0
  }

  def start(): Unit = sensorManager.registerListener(_listener, sensorManager.getDefaultSensor(Sensor.TYPE_LINEAR_ACCELERATION), SensorManager.SENSOR_DELAY_FASTEST)

  def stop(): Unit = {
    sensorManager.unregisterListener(_listener)
    reset()
  }

  def position = Point(_x, _y, _z)

  def reset(): Unit = {
    _listener.reset()
    _x = 0
    _y = 0
    _z = 0
  }

  def onMotion(callback: Point => ()) = _listeners += callback
}
