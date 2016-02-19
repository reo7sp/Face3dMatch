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

package ru.reo7sp.f3m.ui

import android.content.Context
import org.scaloid.common._
import ru.reo7sp.f3m.data.AuthDataStorage
import ru.reo7sp.f3m.image.understand.perspective.Scenery
import ru.reo7sp.f3m.util.AndroidExecutionContext.executionContext

import scala.concurrent.Future

class AuthDataStorageWithUI(implicit ctx: Context) extends AuthDataStorage {
  def loadWithUI: Future[Option[Scenery]] = Future {
    val dialogOpt = spinnerDialog("Загрузка", "Подождите немного")
    val result = load
    dialogOpt.foreach(_.dismiss())
    result
  }

  def saveWithUI(scenery: Scenery): Future[Unit] = Future {
    val dialogOpt = spinnerDialog("Сохранение", "Подождите немного")
    save(scenery)
    dialogOpt.foreach(_.dismiss())
  }
}
