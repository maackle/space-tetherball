package tetherball

import skitch.core.{ResourceLoader, SkitchApp}
import tetherball.states.PlayState
import skitch.core.managed.SkitchState
import java.io.File
import grizzled.slf4j.Logging
import skitch.core.managed.components.Position2D

object Tetherball extends SkitchApp with Logging { self =>

	def initialize = { info("app starting") }
	def cleanup = { info("app closing") }

	val loader = new ResourceLoader(new File("tetherball/src/main/resources"))

	val initialWindowSize = Some(1280, 800)
	val windowTitle = "space tetherball"
	val startState = new PlayState
	val projectionScale = 1f / 30f

	abstract class Thing extends skitch.core.managed.Thing

	run()
}
