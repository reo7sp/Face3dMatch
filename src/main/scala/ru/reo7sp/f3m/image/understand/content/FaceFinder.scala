package ru.reo7sp.f3m.image.understand.content

import ru.reo7sp.f3m.image.Image
import ru.reo7sp.f3m.math.geometry.Rect

trait FaceFinder {
  def apply(image: Image): Traversable[Rect]
}
