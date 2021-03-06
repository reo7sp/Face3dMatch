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

package ru.reo7sp.f3m.math

object NumExtensions {

  implicit class IntExtensions(val i: Int) extends AnyVal {
    def squared = i * i
    def cubed = i * i * i
    def **(exp: Double) = math.pow(i, exp)
    def **(exp: Int) = {
      var r = 1
      var a = i
      var n = exp
      while (n != 0) {
        if ((n & 1) != 0) {
          r *= a
        }
        a *= a
        n >>= 1
      }
      r
    }
    def clamp(min: Int, max: Int) = i min max max min
  }

  implicit class DoubleExtensions(val i: Double) extends AnyVal {
    def squared = i * i
    def cubed = i * i * i
    def **(exp: Double) = math.pow(i, exp)
    def clamp(min: Double, max: Double) = i min max max min
  }

}
