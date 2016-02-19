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

import android.app.Activity
import android.content.Intent
import org.scaloid.common._
import ru.reo7sp.f3m.image.understand.perspective.Scenery
import ru.reo7sp.f3m.util.AndroidExecutionContext.executionContext

import scala.util.{Failure, Success}

class UnlockRequestActivity extends SActivity {
  private[this] val _authDataStorage = new AuthDataStorageWithUI

  private[this] val _unlockCallback = { scenery: Scenery =>
    _authDataStorage.loadWithUI.onComplete {
      case Success(sceneryOpt) => sceneryOpt match {
        case Some(savedScenery) =>
          val similarity = savedScenery similarityWith scenery
          if (similarity > 0.5) {
            setResult(Activity.RESULT_OK, new Intent().putExtra("result", true))
            finish()
          } else {
            setResult(Activity.RESULT_OK, new Intent().putExtra("result", false))
            finish()
          }
        case None =>
          alert("Ошибка", "Эталонная модель не установлена")
          setResult(Activity.RESULT_OK, new Intent().putExtra("result", false))
          finish()
      }
      case Failure(e) =>
        alert("Ошибка", "Эталонная модель не установлена")
        setResult(Activity.RESULT_OK, new Intent().putExtra("result", false))
        finish()
    }
  }

  onCreate {
    val intent = new Intent().putExtra("callbackId", CapturingActivity.queueAction(_unlockCallback))
    intent.start[CapturingActivity]
  }
}
