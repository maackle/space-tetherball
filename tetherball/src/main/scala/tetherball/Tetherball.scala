package tetherball

import skitch.core.{ResourceLoader, SkitchApp}
import tetherball.states.PlayState
import java.io.File
import grizzled.slf4j.Logging
import org.lwjgl.input.Keyboard

object Tetherball extends SkitchApp with Logging { self =>

	def initialize = { info("app starting") }
	def cleanup = { info("app closing") }

	val loader = new ResourceLoader(new File("tetherball/src/main/resources"))

	val initialWindowSize = Some(800, 800)
	val windowTitle = "space tetherball"
	lazy val startState = new PlayState
	val projectionScale = 1f / 30f
	val fps = 60

	object Bits {
		val POLE_BIT = 0x01
		val ROPE_NODE_BIT = 0x02
		val BALL_BIT = 0x04
		val PLAYER_BIT = 0x08
	}

	abstract class Thing extends skitch.core.managed.Thing

	run()
}
