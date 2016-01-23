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

package ru.reo7sp.f3m.image.understand

import ru.reo7sp.f3m.image.{Color, Image, Point}

package object content {
  def findEdges(image: Image)(implicit edgeThreshold: Int = 10): Iterable[Point] = {
    val t = edgeThreshold * edgeThreshold

    val i = image.par
    val a = (i zip i.tail).
      filter {
        case ((_, c1: Color), (_, c2: Color)) => c1.differenceSqr(c2) > t
      } map {
        case ((p1: Point, _), (_, _)) => p1
      }

    val l = image.lines.par
    val b = (l zip l.tail).flatten.
      filter {
        case ((p1: Point, _), (p2: Point, _)) => p1.y != 0 && p2.y != 0
      } map {
        case ((p1: Point, _), (_, _)) => p1
      }

    (a ++ b) seq
  }
}
