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
import org.scaloid.common._
import ru.reo7sp.f3m.data.AuthDataStorage
import ru.reo7sp.f3m.ui.CapturingActivity.FunctionWrapper

class MainActivity extends SActivity {
  private[this] val _authDataStorage = new AuthDataStorage

  onCreate {
    contentView = new SVerticalLayout {
      SButton("Установить", {
        val intent = new Intent().putExtra("callback", FunctionWrapper({ scenery =>
          _authDataStorage.save(scenery)
          toast("Сохранено")
        }))
        intent.start[CapturingActivity]
      })
      SButton("Разблокировать", {
        val intent = new Intent().putExtra("callback", FunctionWrapper({ scenery =>
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
        }))
        intent.start[CapturingActivity]
      })
    }
  }
}
