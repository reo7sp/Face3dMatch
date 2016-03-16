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

package ru.reo7sp.f3m.ui

import android.content.Intent
import android.widget.Button
import org.scaloid.common._
import ru.reo7sp.f3m.R
import ru.reo7sp.f3m.image.understand.perspective.Scenery
import ru.reo7sp.f3m.util.AndroidExecutionContext.executionContext

import scala.util.{Failure, Success}

class MainActivity extends SActivity {
  private[this] val _authDataStorage = new AuthDataStorageWithUI

  private[this] val _installButtonCallback = { scenery: Scenery =>
    _authDataStorage.saveWithUI(scenery).onComplete {
      case Success(_) => alert("Успешно", "Сохранено")
      case Failure(e) => alert("Ошибка", e.toString)
    }
  }

  private[this] val _unlockButtonCallback = { scenery: Scenery =>
    _authDataStorage.loadWithUI.onComplete {
      case Success(sceneryOpt) => sceneryOpt match {
        case Some(savedScenery) =>
          val dialogHandle = spinnerDialog("Сравнение", "Подождите немного")
          val similarity = savedScenery similarityWith scenery
          dialogHandle.foreach(_.dismiss())
          info(s"similarity: ${similarity * 100}%")
          if (similarity > 0.5) {
            alert("Успешно", s"${similarity * 100}%")
          } else {
            alert("Не успешно", s"${similarity * 100}%")
          }
        case None => _installButtonCallback(scenery)
      }
      case Failure(e) => alert("Ошибка", e.toString)
    }
  }

  onCreate {
    setContentView(R.layout.mainactivity)

    find[Button](R.id.setButton).onClick {
      val intent = new Intent().putExtra("callbackId", CapturingActivity.queueAction(_installButtonCallback))
      intent.start[CapturingActivity]
    }
    find[Button](R.id.tryButton).onClick {
      val intent = new Intent().putExtra("callbackId", CapturingActivity.queueAction(_unlockButtonCallback))
      intent.start[CapturingActivity]
    }
  }
}
