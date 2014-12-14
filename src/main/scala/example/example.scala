package org.stelo.glk.example

import org.stelo.glk._

object GlkExample extends GlkApp {

  def glk_main = {
    val vers = glk_gestalt(GlkGestaltIds.gestalt_Version, 0)
    println(s"vers: $vers")
  }

}
