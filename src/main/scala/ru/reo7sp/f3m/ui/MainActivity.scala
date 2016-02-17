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

import android.content.Intent
import android.widget.Button
import org.scaloid.common._
import ru.reo7sp.f3m.R
import ru.reo7sp.f3m.data.AuthDataStorage
import ru.reo7sp.f3m.image.understand.perspective.Scenery

class MainActivity extends SActivity {
  private[this] val _authDataStorage = new AuthDataStorage

  private[this] val _installButtonCallback = { scenery: Scenery =>
    _authDataStorage.save(scenery)
    toast("Сохранено")
  }

  private[this] val _unlockButtonCallback = { scenery: Scenery =>
    _authDataStorage.load match {
      case Some(savedScenery) =>
        val similarity = savedScenery similarityWith scenery
        if (similarity > 0.5) {
          toast(s"Успешно. ${(similarity * 100).toInt}%")
        } else {
          toast(s"Не успешно. ${(similarity * 100).toInt}%")
        }
      case None =>
        _authDataStorage.save(scenery)
        toast("Успешно")
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
