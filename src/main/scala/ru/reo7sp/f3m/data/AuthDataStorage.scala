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

package ru.reo7sp.f3m.data

import java.io.{OutputStreamWriter, PrintWriter}

import android.content.Context
import org.json4s.DefaultFormats
import ru.reo7sp.f3m.image.understand.perspective.Scenery
import ru.reo7sp.f3m.math.geometry.Point

import scala.io.Source
import scala.util.control.NonFatal

class AuthDataStorage(val fileName: String = "authdata.json")(implicit ctx: Context) {
  def load: Option[Scenery] = {
    implicit val formats = DefaultFormats

    try {
//      val points = JsonMethods.parse(Source.fromInputStream(ctx.openFileInput(fileName)).mkString).children.map(_.extract[Point])
      val points = Source.fromInputStream(ctx.openFileInput(fileName)).getLines().map { line =>
        val parts = line.split(' ').map(_.toDouble)
        Point(parts: _*)
      }
      Option(Scenery(points))
    } catch {
      case NonFatal(_) => None
    }
  }

  def save(scenery: Scenery): Unit = {
    implicit val formats = DefaultFormats

    val writer = new PrintWriter(new OutputStreamWriter(ctx.openFileOutput(fileName, Context.MODE_PRIVATE)))
    try {
//      val json = scenery.points.map { point =>
//        ("x" -> point.x) ~ ("y" -> point.y) ~ ("z" -> point.z)
//      }
//      writer.write(JsonMethods.compact(JsonMethods.render(json)))
      scenery.points.foreach { point =>
        writer.println(s"${point.x} ${point.y} ${point.z}")
      }
      writer.flush()
    } finally {
      writer.close()
    }
  }
}
