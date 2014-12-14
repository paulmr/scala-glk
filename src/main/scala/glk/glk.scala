package org.stelo.glk

object GlkTypes {

  sealed class GlkType
  case class glui32(v: Long) extends GlkType
  case class glsi32(v: Int)  extends GlkType
  
  class GlkOpaque(rock: glui32) extends GlkType
    
  object GlkVoid extends GlkType

  type glk_interrupt_handler = () => Unit

  type winid_t   = GlkWindow
  type strid_t   = GlkStream
  type frefid_t  = GlkFileRef
  type schanid_t = GlkSoundChan

  implicit def int2glui32  (i: Int )   = glui32(i.toLong)
  implicit def glui32toInt (g: glui32) = g.v
  implicit def long2glui32 (l: Long)   = glui32(l)
  implicit def glui32ToLong(g: glsi32) = g.v
}

import GlkTypes._

case class GlkWindow   (rock: glui32) extends GlkOpaque(rock)
case class GlkStream   (rock: glui32) extends GlkOpaque(rock)
case class GlkFileRef  (rock: glui32) extends GlkOpaque(rock)
case class GlkSoundChan(rock: glui32) extends GlkOpaque(rock)

case class GlkState(
  val intHandler: Option[glk_interrupt_handler] = None,
  // lists of opaque objects that glk must keep track of
  val windows:    Seq[GlkWindow]    = Nil,
  val streams:    Seq[GlkStream]    = Nil,
  val filerefs:   Seq[GlkFileRef]   = Nil,
  val soundchans: Seq[GlkSoundChan] = Nil
)

object GlkGestaltIds {
  val gestalt_Version = glui32(0)
}

trait GlkFunc {
  val state: GlkState

  val gestalts: List[glui32] = List(0x00000703)

  def glk_tick: Unit = ()

  // opaque object class iterators
  def glk_win_iterate:     Iterator[winid_t]   = state.windows.iterator
  def glk_str_iterate:     Iterator[strid_t]   = state.streams.iterator
  def glk_fref_iterate:    Iterator[frefid_t]  = state.filerefs.iterator
  def glk_schanid_iterate: Iterator[schanid_t] = state.soundchans.iterator

  def glk_gestalt(sel: glui32, `val`: glui32): glui32 = {
    def go(l: List[glui32], count: Long): glui32 = l match {
      case g :: _ if(count == sel) => g
      case g :: rest => go(rest, count + 1)
      case Nil => 0
    }
    go(gestalts, 0)
  }
}

trait GlkApp extends GlkFunc {

  val state = GlkState()

  def glk_main: Unit

  def main(args: Array[String]): Unit = glk_main

}
