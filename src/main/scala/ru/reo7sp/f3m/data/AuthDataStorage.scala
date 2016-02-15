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

package ru.reo7sp.f3m.data

import java.io.OutputStreamWriter

import android.content.Context
import org.json4s.DefaultFormats
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods
import ru.reo7sp.f3m.image.understand.perspective.Scenery
import ru.reo7sp.f3m.math.geometry.Point

import scala.io.Source
import scala.util.control.NonFatal

class AuthDataStorage(val fileName: String = "authdata.json")(implicit ctx: Context) {
  def load: Option[Scenery] = {
    implicit val formats = DefaultFormats
    try { {
      val points = JsonMethods.parse(Source.fromInputStream(ctx.openFileInput(fileName)).mkString).children.map(_.extract[Point])
      Some(Scenery(points))
    }
    } catch {
      case NonFatal(_) => None
    }
  }

  def save(scenery: Scenery): Unit = {
    val writer = new OutputStreamWriter(ctx.openFileOutput(fileName, Context.MODE_PRIVATE))
    try { {
      writer.write(JsonMethods.compact(JsonMethods.render(scenery)))
    }
    } finally {
      writer.close()
    }
  }
}
