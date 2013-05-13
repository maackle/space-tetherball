package tetherball

import skitch.core.{ResourceLoader, SkitchApp}
import tetherball.states.PlayState
import skitch.core.managed.SkitchState
import java.io.File
import grizzled.slf4j.Logging

object Tetherball extends SkitchApp with Logging { self =>

  def initialize = { info("app starting") }
  def cleanup = { info("app closing") }

  val loader = new ResourceLoader(new File("tetherball/src/main/resources"))

  val initialWindowSize = Some(400, 400)
  val windowTitle = "space tetherball"
  val startState = new PlayState

  abstract class State extends SkitchState(this) {
    val loader = self.loader
  }

  run()
}
